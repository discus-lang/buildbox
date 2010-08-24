{-# LANGUAGE PatternGuards, BangPatterns #-}

-- | Running system commands. On some platforms this may cause the command to be executed directly, so 
--   shell tricks won't work. The `Build` monad can be made to log commands executed with all versions
--   of `system` by setting `buildConfigLogSystem` in the `BuildConfig` passed to `runBuildPrintWithConfig`.
--
--   We define a lot of wrappers because executing system commands is the bread-and-butter of 
--   buildbots, and we usually need all the versions...

module BuildBox.Command.System 
	( module System.Exit

	-- * Simple wrappers
	, system
	, ssystem
	, qsystem
	, qssystem
	, ssystemOut
	, qssystemOut

	-- * Tee versions
	, ssystemTee
	, systemTee

	-- * The Core Function
	, systemTeeIO)
where
import BuildBox.Command.System.Internals
import BuildBox.Build
import Control.Concurrent
import System.Process	hiding (system)
import System.Exit
import System.IO
import Control.Monad

debug :: Bool
debug	= False

trace :: String -> IO ()
trace s	= when debug $ putStrLn s


-- Wrappers ---------------------------------------------------------------------------------------
-- | Run a system command, returning its exit code.
system :: String -> Build ExitCode
system cmd
 = do	(code, _, _)		<- systemTee True cmd ""
	return code


-- | Run a successful system command.
--   If it fails throw an error in the `Build` monad.
ssystem :: String -> Build ()
ssystem cmd
 = do	(code, strOut, strErr)	<- systemTee True cmd ""

	when (code /= ExitSuccess)
	 $ throw $ ErrorSystemCmdFailed cmd code strOut strErr


-- | Quietly run a system command, returning its exit code.
qsystem :: String -> Build ExitCode
qsystem cmd
 = do	(code, _, _)		<- systemTee False cmd ""
	return code


-- | Quietly run a successful system command.
--   If it fails thrown an `ErrorSystemCmdFailed` in the `Build` monad.
qssystem :: String -> Build ()
qssystem cmd
 = do	(code, strOut, strErr)	<- systemTee False cmd ""

	when (code /= ExitSuccess)
	 $ throw $ ErrorSystemCmdFailed cmd code strOut strErr
	

-- | Run a successful system command, returning what it wrote to its @stdout@.
--   If anything was written to @stderr@ then treat that as failure. 
--   If it fails due to writing to @stderr@ or returning `ExitFailure`
--   then throw an `ErrorSystemCmdFailed` in the `Build` monad.
ssystemOut :: String -> Build String
ssystemOut cmd
 = do	(code, strOut, strErr)	<- systemTee True cmd ""
	when (code /= ExitSuccess || (not $ null strErr))
	 $ throw $ ErrorSystemCmdFailed cmd code strOut strErr

	return strOut

-- | Quietly run a successful system command, returning what it wrote to its @stdout@.
--   If anything was written to @stderr@ then treat that as failure. 
--   If it fails due to writing to @stderr@ or returning `ExitFailure`
--   then throw an `ErrorSystemCmdFailed` in the `Build` monad.
qssystemOut :: String -> Build String
qssystemOut cmd
 = do	(code, strOut, strErr)	<- systemTee False cmd ""
	when (code /= ExitSuccess || (not $ null strErr))
	 $ throw $ ErrorSystemCmdFailed cmd code strOut strErr

	return strOut



-- Tee versions -----------------------------------------------------------------------------------
ssystemTee  :: Bool -> String -> String -> Build ()
ssystemTee tee cmd strIn
 = do	(code, strOut, strErr)	<- systemTee tee cmd strIn
	when (code /= ExitSuccess)
	 $ throw $ ErrorSystemCmdFailed cmd code strOut strErr



-- | Run a system command, returning its `ExitCode` and what was written to @stdout@ and @stderr@.
--   to the parent while the process runs.
systemTee 
	:: Bool 	-- ^ Whether @stdout@ and @stderr@ are forwarded to the parent.
	-> String	-- ^ Command to run.
	-> String	-- ^ What to write to the commands stdin.
	-> Build (ExitCode, String, String)

systemTee tee cmd strIn
 = do	logSystem cmd
	io $ systemTeeIO tee cmd strIn
	

-- | Like `systemTee` but in the plain `IO` monad.
systemTeeIO :: Bool -> String -> String -> IO (ExitCode, String, String)
systemTeeIO tee cmd strIn
 = do	trace $ "systemTeeIO " ++ show tee ++ ": " ++ cmd

	-- Create some new pipes for the process to write its stdout and stderr to.
	trace $ "systemTeeIO: Creating process"
	(Just hInWrite, Just hOutRead, Just hErrRead, phProc)
	 	<- createProcess 
		$  CreateProcess
			{ cmdspec	= ShellCommand cmd
			, cwd		= Nothing
			, env		= Nothing
			, std_in	= CreatePipe
			, std_out	= CreatePipe
			, std_err	= CreatePipe
			, close_fds	= False }

	-- Push input into in handle
	hPutStr hInWrite strIn
			
	-- To implement the tee-like behavior we'll fork some threads that read lines from the
	-- processes stdout and stderr and write them to these channels. 
	-- 	When they hit EOF they signal this via the semaphores.
	chanOut		<- newChan
	chanErr		<- newChan
	semOut		<- newQSem 0
	semErr		<- newQSem 0

	-- Make duplicates of the above, which will store everything
	-- written to them. This gives us the copy to return from the fn.
	chanOutAcc	<- dupChan chanOut
	chanErrAcc	<- dupChan chanErr

	-- Fork threads to read from the process handles and write to our channels.
	_tidOut		<- forkIO $ streamIn hOutRead chanOut
	_tidErr		<- forkIO $ streamIn hErrRead chanErr

	-- If tee-like behavior is turned on, we forward what the process writes to
	--	its stdout and stderr to the parent.
	_tidStream	<- forkIO $ streamOuts
				[ (chanOut, if tee then Just stdout else Nothing, semOut)
				, (chanErr, if tee then Just stderr else Nothing, semErr) ]

	-- Wait for the main process to complete.
	code		<- waitForProcess phProc
	trace $ "systemTeeIO: Process done, code = " ++ show code

	trace $ "systemTeeIO: Waiting for sems"
	-- Wait for the tee processes to finish.
	--	We need to do this to avoid corrupted output on the console due to our forwarding
	--	threads writing at the same time as successing Build commands.
	mapM_ waitQSem [semOut, semErr]

	trace $ "systemTeeIO: Getting output"
	-- Get what was written to its stdout and stderr.
	--	getChanContents is a lazy read, so don't pull from the channel after
	--	seeing a Nothing else we'll block forever.
	strOut		<- liftM (concat . slurpUntilNothing) $ getChanContents chanOutAcc
	strErr		<- liftM (concat . slurpUntilNothing) $ getChanContents chanErrAcc

	trace $ "systemTeeIO stdout: " ++ strOut
	trace $ "systemTeeIO stderr: " ++ strErr

	trace $ "systemTeeIO: All done"
	code `seq` strOut `seq` strErr `seq` 
		return	(code, strOut, strErr)



slurpUntilNothing :: [Maybe a] -> [a]
slurpUntilNothing xx
 = case xx of
	[]		-> []
	Nothing : _	-> []
	Just x  : xs	-> x : slurpUntilNothing xs

