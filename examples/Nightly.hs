
import BuildBox

main 
 = do	result	<- runBuildAndPrintResult build
	return ()
	
build
 = do	sleep 1
	system "ls"

	outCheckOk "Checking we can talk to google"
	 $ HostReachable "www.google.com"

	outCheckOk "Checking we can talk to mars"
	 $ HostReachable "mars.wibble"