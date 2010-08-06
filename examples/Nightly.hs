
import BuildBox

main 
 = do	result	<- runBuildAndPrintResult build
	return ()
	
build
 = do	platform	<- getHostPlatform
	versionGHC	<- getVersionGHC
	versionGCC	<- getVersionGCC

	outLn $ pprPlatform platform	
	outLn $ "GHC version: " ++ versionGHC
	outLn $ "GCC version: " ++ versionGCC
	
	out "\n"
	
	outCheckOk "Check Google is reachable"
	 $ HostReachable "www.google.com"

	outCheckOk "Check code.haskell.org is reachable"
	 $ HostReachable "code.haskell.org"
	
	outCheckOk "Check code.haskell.org web server is up"
	 $ UrlGettable "http://code.haskell.org"
	