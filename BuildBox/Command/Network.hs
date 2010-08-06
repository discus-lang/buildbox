
-- | Network utilities.
module BuildBox.Command.Network
	(PropNetwork(..))
where
import BuildBox.Build
import BuildBox.Command.File
import BuildBox.Command.System
import System.Exit

type HostName	= String
type URL	= String

data PropNetwork

	-- | Use ping to check if a host is reachable.
	= HostReachable	HostName

	-- | Use wget to check if a web-page is gettable.
	--   The page is deleted after downloading.
	| UrlGettable	URL

	deriving Show
	
instance Testable PropNetwork where
 test prop
  = case prop of

	-- Works on OSX 10.6.2
	-- -o               exit successfully after recieving one reply packet.
	HostReachable hostName
	 -> do	check	$ HasExecutable "ping"
		code	<- systemNullCode $ "ping -o " ++ hostName
		return $ code == ExitSuccess
		
	-- Works on OSX 10.6.2, wget 1.12
	--  -q              quiet
	--  --delete-after  delete page after downloading.
	UrlGettable url
	 -> do	check	$ HasExecutable "wget"
		code 	<- systemNullCode $ "wget -q --delete-after " ++ url
		return	$ code == ExitSuccess
		


	

