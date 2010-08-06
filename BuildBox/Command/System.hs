{-# LANGUAGE PatternGuards #-}

module BuildBox.Command.System 
	( module System.Exit
	, system
	, systemCode
	, systemNullCode
	, systemWithStdout)
where
import BuildBox.Build
import System.Process	hiding (system)
import System.Exit
import System.IO
import System.Posix.IO
import qualified System.Cmd


-- | Run a system command, expecting it to succeed.
system :: String -> Build ()
system cmd
 = do	code	<- io $ System.Cmd.system cmd
	case code of
	 ExitSuccess   -> return ()
	 ExitFailure _ -> throw $ ErrorSystemCmdFailed cmd


-- | Run a system command, returning its exitcode.
systemCode :: String -> Build ExitCode
systemCode cmd
 = do	code	<- io $ System.Cmd.system cmd
	return code

	
-- | Run a system command, discarding its output and returning its exitcode.
systemNullCode :: String -> Build ExitCode
systemNullCode cmd
 = do	code	<- io $ System.Cmd.system $ cmd ++ " > /dev/null 2> /dev/null"
	return code
	

-- | Run a system command, capturing its stdout.
--   Expect success, and nothing written to its stderr.
--
systemWithStdout :: String -> Build String
systemWithStdout cmd
 | prog : args	<- words cmd
 = do	(fdOutRead, fdOutWrite)	<- io $ createPipe
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
	