
module BuildBox.Command.System 
	( system
	, systemCode
	, systemNullCode)
where
import BuildBox.Build
import System.Exit
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