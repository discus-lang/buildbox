
-- | Working with the network.
module BuildBox.Command.Network
	(PropNetwork(..))
where
import BuildBox.Build
import BuildBox.Command.File
import BuildBox.Command.System

type HostName	= String
type URL	= String

data PropNetwork

	-- | The given host is reachable with @ping@.
	= HostReachable	HostName

	-- | Use @wget@ to check if a web-page is gettable.
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
		code	<- qsystem $ "ping -o " ++ hostName
		return $ code == ExitSuccess
		
	-- Works on OSX 10.6.2, wget 1.12
	--  -q              quiet
	--  --delete-after  delete page after downloading.
	UrlGettable url
	 -> do	check	$ HasExecutable "wget"
		code 	<- qsystem $ "wget -q --delete-after " ++ url
		return	$ code == ExitSuccess
		


	

