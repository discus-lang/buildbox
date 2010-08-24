{-# LANGUAGE PatternGuards #-}
module BuildBox.Command.System.Internals
	( streamIn
	, streamOuts)
where
import System.IO
import Control.Concurrent	

-- | Continually read lines from a handle and write them to this channel.
--   When the handle hits EOF then write `Nothing` to the channel.
streamIn  :: Handle -> Chan (Maybe String) -> IO ()
streamIn hRead chan
 = do	eof	<- hIsEOF hRead
	if eof
	 then do
		writeChan chan Nothing
		return ()
		
	 else do
		str	<- hGetLine hRead
		writeChan chan (Just (str ++ "\n"))
		streamIn hRead chan


-- | Continually read lines from some channels and write them to handles.
--   When all the channels return `Nothing` then we're done.
--   When we're done, signal this fact on the semaphore.
streamOuts :: [(Chan (Maybe String), (Maybe Handle), QSem)] -> IO ()
streamOuts chans 
 = streamOuts' False [] chans

 where	-- we're done.
	streamOuts' _ []   []	
		= return ()

	-- play it again, sam.
	streamOuts' True prev []	
	 = 	streamOuts' False [] prev

	streamOuts' False prev []
	 = do	yield
		streamOuts' False [] prev

	-- try to read from the current chan.
	streamOuts' active prev (x@(chan, mHandle, qsem) : rest)
	 = isEmptyChan chan >>= \empty -> 
	   if empty 
	    then streamOuts' active (prev ++ [x]) rest
	    else do
		mStr	<- readChan chan
		case mStr of
		 Nothing	
		  -> do	signalQSem qsem
			streamOuts' active prev rest

		 Just str 
		  | Just h	<- mHandle
		  -> do	hPutStr h str
			streamOuts' True (prev ++ [x]) rest

		  | otherwise
		  -> 	streamOuts' True (prev ++ [x]) rest