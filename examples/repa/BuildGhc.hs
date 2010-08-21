
-- | Build stages concerning GHC.
module BuildGhc
	( ghcUnpack
	, ghcBuild
	, ghcLibs)
where
import Benchmarks
import Config
import BuildBox
import Data.Time
import Control.Monad


ghcUnpack config
 = inDir (configScratchDir config)
 $ do	outLn "* Unpacking GHC"
	outCheckFalseOk " - Checking build directory is empty"
	 $ HasDir $ " -ghc-head"

	let Just snapshot = configWithGhcSnapshot config

	outLn $ " - Unpacking snapshot " ++ snapshot
	system $ "tar zxf " ++ snapshot
	
	outLn $ " - Updating snapshot"
	inDir "ghc-head"
	 $ system "./darcs-all pull -av"
	

ghcBuild config
 = inDir (configScratchDir config)
 $ inDir "ghc-head"
 $ do	outLn "* Building GHC"
	
	system $ "perl boot"
	system $ "./configure"
	system $ "make"
	
	inDir "inplace/bin"
	 $ system $ "ln -s ghc-stage2 ghc"


ghcLibs config
 = do	let ghcPkg	= configWithGhcPkg
	outLn "* Building base libraries."
	outCheckOk " - Checking for cabal"
	 $ HasExecutable "cabal"
	
	let cabal	= "cabal "
			++ " --with-compiler=" ++ configWithGhc config
			++ " --with-hc-pkg="   ++ configWithGhcPkg config
	
	let cabalInstall pkg
		= do	outLn   $ "- Building " ++ pkg
			system	$ cabal ++ " install " ++ pkg
			outBlank
		
	mapM_ cabalInstall basePackages
