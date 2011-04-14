{-# LANGUAGE PatternGuards, BangPatterns #-}
module BuildBox.Command.System.Internals
	( streamIn
	, streamOuts)
where
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Data.ByteString.Char8		(ByteString)
import qualified Data.ByteString.Char8	as BS	


-- | Continually read lines from a handle and write them to this channel.
--   When the handle hits EOF then write `Nothing` to the channel.
streamIn  :: Handle -> TChan (Maybe ByteString) -> IO ()
streamIn !hRead !chan
 = do	eof	<- hIsEOF hRead
	if eof
	 then do
		atomically $ writeTChan chan Nothing
		return ()
		
	 else do
		str	<- BS.hGetLine hRead
		atomically $ writeTChan chan (Just str)
		streamIn hRead chan


-- | Continually read lines from some channels and write them to handles.
--   When all the channels return `Nothing` then we're done.
--   When we're done, signal this fact on the semaphore.
streamOuts :: [(TChan (Maybe ByteString), (Maybe Handle), QSem)] -> IO ()
streamOuts !chans 
 = streamOuts' False [] chans

	-- There doesn't seem to be a way to perform a unix-style "select" on channels.
	-- We want to wait until any of the channels becomes available for reading.
	-- We're doing this just by polling them each in turn, and waiting a bit
	--	if none of them had any data.
		
 where	-- we're done.
	streamOuts' _ []   []	
		= return ()

	-- play it again, sam.
	streamOuts' True prev []	
	 = 	streamOuts' False [] prev

	streamOuts' False prev []
	 = do	threadDelay 100000
		streamOuts' False [] prev

	-- try to read from the current chan.
	streamOuts' !active !prev (!x@(!chan, !mHandle, !qsem) : rest)
	 = do	
		-- try and read a string from the channel, but don't block
		-- if there aren't any.
		mStr	<- atomically
			$  do	isEmpty	<- isEmptyTChan chan
				if isEmpty 
				 then    return (False, Nothing)

				 else do mStr	<- readTChan chan
					 return (True, mStr)
				
		case mStr of
		 (False, _)
		  -> streamOuts' active (prev ++ [x]) rest

		 (True, Nothing)
		  -> do	signalQSem qsem
			streamOuts' active prev rest

		 (True, Just str)
		  | Just h	<- mHandle
		  -> do	BS.hPutStr h str
			hPutChar   h '\n'
			streamOuts' True (prev ++ [x]) rest

		  | otherwise
		  -> 	streamOuts' True (prev ++ [x]) rest					

