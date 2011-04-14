{-# LANGUAGE PatternGuards, BangPatterns #-}

-- | Running system commands. On some platforms this may cause the command to be executed directly, so 
--   shell tricks won't work. The `Build` monad can be made to log commands executed with all versions
--   of `system` by setting `buildConfigLogSystem` in the `BuildConfig` passed to `runBuildPrintWithConfig`.
--
--   We define a lot of wrappers because executing system commands is the bread-and-butter of 
--   buildbots, and we usually need all the versions...

module BuildBox.Command.System 
	( module System.Exit

	-- * Wrappers
	, system
	, ssystem
	, qsystem
	, qssystem
	, ssystemOut
	, qssystemOut
	, systemTee
	, systemTeeLog
	, ssystemTee
	, systemTeeIO

	-- * The real function
	, systemTeeLogIO)
where
import BuildBox.Command.System.Internals
import BuildBox.Build
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import System.Exit
import System.IO
import Data.ByteString.Char8		(ByteString)
import BuildBox.Data.Log		(Log)
import System.Process			hiding (system)
import qualified BuildBox.Data.Log	as Log

debug :: Bool
debug	= False

trace :: String -> IO ()
trace s	= when debug $ putStrLn s


-- Wrappers ---------------------------------------------------------------------------------------
-- | Run a system command, returning its exit code.
system :: String -> Build ExitCode
system cmd
 = do	(code, _, _)		<- systemTeeLog True cmd Log.empty
	return code


-- | Run a successful system command.
--   If the exit code is `ExitFailure` then throw an error in the `Build` monad.
ssystem :: String -> Build ()
ssystem cmd
 = do	(code, logOut, logErr)	<- systemTeeLog True cmd Log.empty

	when (code /= ExitSuccess)
	 $ throw $ ErrorSystemCmdFailed cmd code logOut logErr


-- | Quietly run a system command, returning its exit code.
qsystem :: String -> Build ExitCode
qsystem cmd
 = do	(code, _, _)		<- systemTeeLog False cmd Log.empty
	return code


-- | Quietly run a successful system command.
--   If the exit code is `ExitFailure` then throw an error in the `Build` monad.
qssystem :: String -> Build ()
qssystem cmd
 = do	(code, logOut, logErr)	<- systemTeeLog False cmd Log.empty

	when (code /= ExitSuccess)
	 $ throw $ ErrorSystemCmdFailed cmd code logOut logErr
	

-- | Run a successful system command, returning what it wrote to its @stdout@.
--   If anything was written to @stderr@ then treat that as failure. 
--   If it fails due to writing to @stderr@ or returning `ExitFailure`
--   then throw an error in the `Build` monad.
ssystemOut :: String -> Build String
ssystemOut cmd
 = do	(code, logOut, logErr)	<- systemTeeLog True cmd Log.empty
	when (code /= ExitSuccess || (not $ Log.null logErr))
	 $ throw $ ErrorSystemCmdFailed cmd code logOut logErr

	return $ Log.toString logOut

-- | Quietly run a successful system command, returning what it wrote to its @stdout@.
--   If anything was written to @stderr@ then treat that as failure. 
--   If it fails due to writing to @stderr@ or returning `ExitFailure`
--   then throw an error in the `Build` monad.
qssystemOut :: String -> Build String
qssystemOut cmd
 = do	(code, logOut, logErr)	<- systemTeeLog False cmd Log.empty
	when (code /= ExitSuccess || (not $ Log.null logErr))
	 $ throw $ ErrorSystemCmdFailed cmd code logOut logErr

	return $ Log.toString logOut



-- Tee versions -----------------------------------------------------------------------------------

-- | Like `systemTeeIO`, but in the `Build` monad.
systemTee :: Bool -> String -> String -> Build (ExitCode, String, String)
systemTee tee cmd strIn
 = do	logSystem cmd
	io $ systemTeeIO tee cmd strIn

-- | Like `systemTeeLogIO`, but in the `Build` monad.
systemTeeLog :: Bool -> String -> Log -> Build (ExitCode, Log, Log)
systemTeeLog tee cmd logIn
 = do	logSystem cmd
	io $ systemTeeLogIO tee cmd logIn


-- | Like `systemTeeIO`, but in the `Build` monad and throw an error if it returns `ExitFailure`.
ssystemTee  :: Bool -> String -> String -> Build ()
ssystemTee tee cmd strIn
 = do	(code, logOut, logErr)	<- systemTeeLog tee cmd (Log.fromString strIn)
	when (code /= ExitSuccess)
	 $ throw $ ErrorSystemCmdFailed cmd code logOut logErr


-- | Like `systemTeeLogIO`, but with strings.
systemTeeIO :: Bool -> String -> String -> IO (ExitCode, String, String)
systemTeeIO tee cmd strIn
 = do	(code, logOut, logErr)	<- systemTeeLogIO tee cmd $ Log.fromString strIn
	return	(code, Log.toString logOut, Log.toString logErr)


-- | Run a system command, returning its `ExitCode` and what was written to @stdout@ and @stderr@.
systemTeeLogIO
	:: Bool 	-- ^ Whether @stdout@ and @stderr@ should be forwarded to the parent process.
	-> String 	-- ^ Command to run.
	-> Log		-- ^ What to pass to the command's @stdin@.
	-> IO (ExitCode, Log, Log)

systemTeeLogIO tee cmd logIn
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

	-- Push input into in handle. Close the handle afterwards to ensure the
	-- process gets sent the EOF character.
	hPutStr hInWrite $ Log.toString logIn
	hClose  hInWrite
			
	-- To implement the tee-like behavior we'll fork some threads that read lines from the
	-- processes stdout and stderr and write them to these channels. 
	-- 	When they hit EOF they signal this via the semaphores.
	chanOut		<- newTChanIO
	chanErr		<- newTChanIO
	semOut		<- newQSem 0
	semErr		<- newQSem 0

	-- Make duplicates of the above, which will store everything
	-- written to them. This gives us the copy to return from the fn.
	chanOutAcc	<- atomically $ dupTChan chanOut
	chanErrAcc	<- atomically $ dupTChan chanErr

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
	logOut		<- slurpChan chanOutAcc Log.empty
	logErr		<- slurpChan chanErrAcc Log.empty

	trace $ "systemTeeIO stdout: " ++ Log.toString logOut
	trace $ "systemTeeIO stderr: " ++ Log.toString logErr

	trace $ "systemTeeIO: All done"
	code `seq` logOut `seq` logErr `seq` 
		return	(code, logOut, logErr)

slurpChan :: TChan (Maybe ByteString) -> Log -> IO Log
slurpChan !chan !ll
 = do	mStr	<- atomically $ readTChan chan
	case mStr of
	 Nothing	-> return ll
	 Just str	-> slurpChan chan (ll Log.|> str)

