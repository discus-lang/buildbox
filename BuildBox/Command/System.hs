{-# LANGUAGE PatternGuards #-}

-- | Running system commands. On some platforms this may cause the command to be executed directly, so 
--   shell tricks won't work. The `Build` monad can be made to log all commands executed with versions
--   of `system` by setting `buildConfigLogSystem` in the `BuildConfig` passed to `runBuildPrintWithConfig`.
module BuildBox.Command.System 
	( module System.Exit
	, system
	, systemCode
	, systemNull
	, systemNullCode
	, systemWithStdout)
where
import BuildBox.Build
import System.Process	hiding (system)
import System.Exit
import System.IO
import System.Posix.IO
import qualified System.Cmd


-- | Run a system command, expecting success.
system :: String -> Build ()
system cmd
 = do	logSystem cmd
	code	<- io $ System.Cmd.system cmd
	case code of
	 ExitSuccess   -> return ()
	 ExitFailure _ -> throw $ ErrorSystemCmdFailed cmd


-- | Run a system command, returning its exit code.
systemCode :: String -> Build ExitCode
systemCode cmd
 = do	logSystem cmd
	code	<- io $ System.Cmd.system cmd
	return code


-- | Run a system command, expecting success, discarding anything written to @stdout@ or @stderr@.
systemNull :: String -> Build ()
systemNull cmd
 = do	let cmd'	= cmd ++ " > /dev/null 2> /dev/null"
	logSystem cmd'
	code		<- io $ System.Cmd.system $ cmd'
	case code of
	 ExitSuccess	-> return ()
	 ExitFailure _	-> throw $ ErrorSystemCmdFailed cmd'
	
	
-- | Run a system command, returning its exit code, discarding anything written to @stdout@ or @stderr@.
systemNullCode :: String -> Build ExitCode
systemNullCode cmd
 = do	let cmd'	= cmd ++ " > /dev/null 2> /dev/null"
	logSystem cmd'
	code	<- io $ System.Cmd.system cmd'
	return code
	

-- | Run a system command, expecting success. 
--   If anything was written to @stderr@ then treat that as failure, otherwise return what
--   was written to @stdout@.
systemWithStdout :: String -> Build String
systemWithStdout cmd
 | prog : args	<- words cmd
 = do	logSystem cmd
	(fdOutRead, fdOutWrite)	<- io $ createPipe
	hOutRead		<- io $ fdToHandle fdOutRead
	hOutWrite		<- io $ fdToHandle fdOutWrite

	(fdErrRead, fdErrWrite)	<- io $ createPipe
	hErrRead		<- io $ fdToHandle fdErrRead
	hErrWrite		<- io $ fdToHandle fdErrWrite

	-- run the command
 	ph	<- io $ runProcess prog args Nothing Nothing 
			Nothing (Just hOutWrite) (Just hErrWrite)

	code	<- io $ waitForProcess ph

	-- grab its stdout
	strOut	<- io $ hGetContents hOutRead
	strErr	<- io $ hGetContents hErrRead

	if code == ExitSuccess && null strErr
	 then return strOut
	 else throw $ ErrorSystemCmdFailed cmd
	
 | otherwise
 = error "systemWithStdout: empty command"

