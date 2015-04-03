{-# LANGUAGE PatternGuards, BangPatterns, NamedFieldPuns #-}
module BuildBox.Command.System.Internals
        ( streamIn
        , streamOuts)
where
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Monad
import Foreign.Ptr
import Data.IORef
import Data.Char
import Data.Word
import Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Internal       as BS
import qualified Data.ByteString.Char8          as BS   

import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import GHC.IO.Buffer
import GHC.IO.BufferedIO as Buffered

import Foreign.Marshal.Utils    (copyBytes)


-- | Continually read lines from a handle and write them to this channel.
--   When the handle hits EOF then write `Nothing` to the channel.
streamIn  :: Handle -> TChan (Maybe ByteString) -> IO ()
streamIn !hRead !chan
 = streamIn' False hRead chan

streamIn' 
        :: Bool                         -- ^ whether the last line ended in a newline character
        -> Handle                       -- ^ handle to read lines from
        -> TChan (Maybe ByteString)     -- ^ channel to write lines to
        -> IO ()

streamIn' !gotNewLine !hRead !chan
 = uponM (hIsEOF hRead)
    -- On EOF, if the last line ended in a newline then write a final
    -- empty ByteString to the channel to signify this.
    (do when gotNewLine
         $ atomically $ writeTChan chan (Just BS.empty)
        atomically $ writeTChan chan Nothing
        return ())

    -- Block until there is a line available from the channel.
    -- The line may or may not end in a newline character, depending
    -- on whether there was a literal newline in the source file, 
    -- or the file was flushed.
    (do str     <- hGetLineNL hRead     

        if BS.null str 
         then   -- hmm. The file was supposedly non-empty, but we didn't get
                -- any data. Let's just try again.
                streamIn' gotNewLine hRead chan

         else do
                -- Check whether we got an actual newline character on the
                -- end of the string.
                let hasNewLine
                     | BS.last str == '\n'        = True
                     | otherwise                  = False
                 
                -- For string ending in newline characters, we don't want to
                -- push the newline to the consumer, but we need to remember
                -- if we've seen one to handle the end-of-file condition properly.
                let str'
                     | hasNewLine                 = BS.init str
                     | otherwise                  = str
                           
                atomically $ writeTChan chan (Just str')
                streamIn' hasNewLine hRead chan)
 
uponM :: Monad m => m Bool -> m a -> m a -> m a
uponM c x y
 = do   b       <- c
        if b then x else y

               
-- | Continually read lines from some channels and write them to handles.
--   When all the channels return `Nothing` then we're done.
--   When we're done, signal this fact on the semaphore.
streamOuts :: [(TChan (Maybe ByteString), (Maybe Handle), QSem)] -> IO ()
streamOuts !chans 
 = streamOuts' False [] chans

        -- There doesn't seem to be a way to perform a unix-style "select" on channels.
        -- We want to wait until any of the channels becomes available for reading.
        -- We're doing this just by polling them each in turn, and waiting a bit
        --      if none of them had any data.
                
 where  -- we're done.
        streamOuts' _ []   []   
                = return ()

        -- play it again, sam.
        streamOuts' True prev []        
         =      streamOuts' False [] prev

        streamOuts' False prev []
         = do   threadDelay 1000
                yield
                streamOuts' False [] prev
 
        -- try to read from the current chan.
        streamOuts' !active !prev (!x@(!chan, !mHandle, !qsem) : rest)
         = do   
                -- try and read a string from the channel, but don't block
                -- if there aren't any.
                mStr    <- atomically
                        $  do   isEmpty <- isEmptyTChan chan
                                if isEmpty 
                                 then    return (False, Nothing)

                                 else do mStr   <- readTChan chan
                                         return (True, mStr)
                                
                case mStr of
                 (False, _)
                  -> streamOuts' active (prev ++ [x]) rest

                 (True, Nothing)
                  -> do signalQSem qsem
                        streamOuts' active prev rest

                 (True, Just str)
                  | Just h      <- mHandle
                  -> do BS.hPutStr h str
                        hPutChar   h '\n'
                        streamOuts' True (prev ++ [x]) rest

                  | otherwise
                  ->    streamOuts' True (prev ++ [x]) rest                                     


-- Code hacked out of Data.ByteString library.
-- | Like `hGetLine`, but return the newline charater on the end of the string, 
--   if there was one. This allows us to distinguish between lines that were flushed
--   from the IO buffer due to a newline character, and partial lines that were
--   flushed manually, and don't have a newline.
hGetLineNL :: Handle -> IO ByteString
hGetLineNL h =
  wantReadableHandle_ "BuildBox.Command.System.Internals" h $
    \ h_@Handle__{haByteBuffer} -> do
      flushCharReadBuffer h_
      buf <- readIORef haByteBuffer
      if isEmptyBuffer buf
         then fill h_ buf 0 []
         else haveBuf h_ buf 0 []
 where

  fill h_@Handle__{haByteBuffer,haDevice} buf len xss =
    len `seq` do
    (r,buf') <- Buffered.fillReadBuffer haDevice buf
    if r == 0
       then do writeIORef haByteBuffer buf{ bufR=0, bufL=0 }
               if len > 0
                  then mkBigPS len xss
                  else ioe_EOF
       else haveBuf h_ buf' len xss

  haveBuf h_@Handle__{haByteBuffer}
          buf@Buffer{ bufRaw=raw, bufR=w, bufL=r }
          len xss =
    do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs  <- mkPS raw r off

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
         then do 
                if (w == off + 1)
                  then writeIORef haByteBuffer buf{ bufL=0, bufR=0 }
                  else writeIORef haByteBuffer buf{ bufL = off + 1 }

                -- Produce the final string.
                -- If r == w then we've found the end-of file, but there
                -- was no newline character on the input.
                if r == w
                  then mkBigPS new_len (xs:xss)
                  else mkBigPS new_len (BS.pack "\n" : xs : xss)

         else do
                fill h_ buf{ bufL=0, bufR=0 } new_len (xs:xss)

  -- Try to find an end-of-line character in the buffer, if there is one.
  -- If not we'll have r == w.
  findEOL r w raw
        | r == w        = return w
        | otherwise 
        = do
            c <- readWord8Buf raw r
            if c == fromIntegral (ord '\n')
                then return  r
                else findEOL (r+1) w raw


mkPS :: RawBuffer Word8 -> Int -> Int -> IO ByteString
mkPS buf start end =
 BS.create len $ \p ->
   withRawBuffer buf $ \pbuf -> do
   copyBytes p (pbuf `plusPtr` start) len
 where
   len = end - start

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [ps]  = return ps
mkBigPS _ pss   = return $! BS.concat (reverse pss)

