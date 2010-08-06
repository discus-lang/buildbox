
-- | Network utilities.
module BuildBox.Command.Network
	(PropNetwork(..))
where
import BuildBox.Build
import BuildBox.Command.System
import System.Exit

type HostName	= String

data PropNetwork
	= HostReachable HostName
	deriving Show
	
instance Testable PropNetwork where
 test prop
  = case prop of
	HostReachable hostName
	 -> do	code	<- systemNullCode $ "ping -o " ++ hostName
		return $ code == ExitSuccess
		
	

