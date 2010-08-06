
module BuildBox.Command.Environment
	( -- * Build platform
	  Platform(..)
	, pprPlatform
	, getHostPlatform
	, getHostName
	, getHostArch
	, getHostProcessor
	, getHostOS
	, getHostRelease
	
	  -- * Software versions
	, getVersionGHC
	, getVersionGCC)
where
import BuildBox.Build
import BuildBox.Command.System
import BuildBox.Command.File


-- | Collect all the generic info about the platform we're running on.
data Platform
	= Platform
	{ platformHostName 	:: String
	, platformHostArch	:: String
	, platformHostProcessor	:: String
	, platformHostOS	:: String
	, platformHostRelease	:: String
	}
	deriving Show
	
	
-- | Pretty print a platform.
pprPlatform :: Platform -> String
pprPlatform plat
 = unlines
	[ "  host       : " ++ platformHostName plat
	, "  arch       : " ++ platformHostArch plat
	, "  processor  : " ++ platformHostProcessor plat
	, "  system     : " ++ platformHostOS plat ++ " " ++ platformHostRelease plat ]
	
	
-- | Get information about the host platform.
getHostPlatform :: Build Platform
getHostPlatform 
 = do	name		<- getHostName
	arch		<- getHostArch
	processor	<- getHostProcessor
	os		<- getHostOS
	release		<- getHostRelease
	
	return	$ Platform
		{ platformHostName	= name
		, platformHostArch	= arch
		, platformHostProcessor	= processor
		, platformHostOS	= os
		, platformHostRelease	= release }
		

-- Platform Tests ---------------------------------------------------------------------------------
-- | Get the name of this host.
getHostName :: Build String
getHostName 	
 = do	check $ HasExecutable "uname"
	name	<- systemWithStdout "uname -n"
	return	$ init name


-- | Get the host architecture.
getHostArch :: Build String
getHostArch
 = do	check $ HasExecutable "arch"
	name	<- systemWithStdout "arch"
	return	$ init name


-- | Get the host processor name
getHostProcessor :: Build String
getHostProcessor
 = do	check $ HasExecutable "uname"
	name	<- systemWithStdout "uname -p"
	return	$ init name


-- | Get the host operating system
getHostOS :: Build String
getHostOS
 = do	check $ HasExecutable "uname"
	os	<- systemWithStdout "uname -s"
	return	$ init os


-- | Get the host operating system release
getHostRelease :: Build String
getHostRelease
 = do	check $ HasExecutable "uname"
	str	<- systemWithStdout "uname -r"
	return	$ init str
	
-- Software version tests -------------------------------------------------------------------------
-- | Get the GHC version
getVersionGHC :: Build String
getVersionGHC 
 = do	check $ HasExecutable "ghc"
	str	<- systemWithStdout "ghc --version"
	return	$ init str
	
-- | Get the GCC version
getVersionGCC :: Build String
getVersionGCC
 = do	check $ HasExecutable "gcc"
 	str	<- systemWithStdout "gcc --version"
	return	$ head $ lines str


