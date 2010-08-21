{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

-- Repa buildbot
-- 	Used to automate building and performance testing of GHC and Repa
--
import BuildBox
import Args
import Config
import BuildRepa
import BuildGhc
import Control.Monad
import System.Console.ParseArgs
import Data.Time
import Data.List
import Data.Maybe

main 
 = do	args	<- parseArgsIO ArgsTrailing buildArgs
	mainWithArgs args

-- | Decide what to do
mainWithArgs args

	-- Print usage help
	| gotArg args ArgHelp
	= usageError args ""

	-- Dump a results file.
	| Just fileName	<- getArg args ArgDoDump
	, []		<- argsRest args
	= do	contents	<- readFile fileName
		let results	=  (read contents) :: BuildResults
		putStrLn $ render $ ppr results

	-- Compare two results files.
	| gotArg args ArgDoCompare
	= do	let fileNames	= argsRest args
		contentss	<- mapM readFile fileNames
		let (results :: [BuildResults])
				= map read contentss
		
		let [baseline, current] 
				= map buildResultBench results

		putStrLn $ show $ render $ pprComparisons baseline current
		
	-- Do some building or testing process.
	| or $ map (gotArg args) 
		[ ArgDoNightly
		, ArgDoGhcUnpack,  ArgDoGhcBuild,  ArgDoGhcLibs
		, ArgDoRepaUnpack, ArgDoRepaBuild, ArgDoRepaTest]
	= do	
		let tmpDir = fromMaybe 
				(error "You must specify --scratch-dir with --repa-unpack, --repa-build or --repa-test.")
				(getArg args ArgScratchDir)

		let config = slurpConfig args tmpDir

		result	<- runBuildAndPrintResult (nightly config)
		return ()

	| otherwise
	= usageError args "Nothing to do...\n"


-- | Slurp configuration information from the command line arguments.
slurpConfig args tmpDir
 = let Just iterations	= getArg args ArgTestIterations

   in  Config
	{ configVerbose		= gotArg args ArgVerbose
	, configScratchDir	= tmpDir
	, configWithGhcBuild	= getArg args ArgWithGhcBuild

	, configWithGhc 	= maybe "ghc" (\dir -> dir ++ "/inplace/bin/ghc-stage2") 
				$ getArg args ArgWithGhcBuild

	, configWithGhcPkg	= maybe "ghc-pkg" (\dir -> dir ++ "/inplace/bin/ghc-pkg") 
				$ getArg args ArgWithGhcBuild

	, configWithGhcSnapshot	
		= if gotArg args ArgDoGhcUnpack
		    then getArg args ArgDoGhcUnpack
		    else getArg args ArgWithGhcSnapshot	

	-- What stages to run.
	-- If --nightly is set then do them all.
	, configDoGhcUnpack	= gotArg args ArgDoGhcUnpack  || gotArg args ArgDoNightly
	, configDoGhcBuild	= gotArg args ArgDoGhcBuild   || gotArg args ArgDoNightly
	, configDoGhcLibs	= gotArg args ArgDoGhcLibs    || gotArg args ArgDoNightly
	, configDoRepaUnpack	= gotArg args ArgDoRepaUnpack || gotArg args ArgDoNightly
	, configDoRepaBuild	= gotArg args ArgDoRepaBuild  || gotArg args ArgDoNightly
	, configDoRepaTest	= gotArg args ArgDoRepaTest   || gotArg args ArgDoNightly

	-- Testing config.
	, configIterations	= iterations 
	, configWriteResults	= getArg args ArgWriteResults
	, configAgainstResults	= getArg args ArgAgainstResults

	-- TODO: check we have both args
	, configMailFromTo	= let result	
					| Just from	<- getArg args ArgMailFrom
					, Just to	<- getArg args ArgMailTo
					= Just (from, to)
							
					| otherwise
					= Nothing
				  in	result
	}


-- | The nightly build.
--   This only runs the stages set in the config.
nightly :: Config -> Build ()
nightly config
 = do	outLine
	outLn "Repa BuildBot\n"
	
	-- Check the current environment.
	env	<- getEnvironmentWith 
			[ ("GHC", getVersionGHC $ configWithGhc config)
			, ("GCC", getVersionGCC "gcc") ]
			
	outLn $ render $ ppr $ env
	
	outLine
	outBlank
	
	-- Unpack GHC
	when (configDoGhcUnpack config)
	 $ ghcUnpack config
	
	-- If we've been told to build GHC, then use
	-- 	the completed build as the default compiler.
	configNew
	  <- if configDoGhcBuild config
	      then do ghcBuild config
		      return config
				{ configWithGhc	   = configScratchDir config ++ "/ghc-head/inplace/bin/ghc-stage2"
				, configWithGhcPkg = configScratchDir config ++ "/ghc-head/inplace/bin/ghc-pkg" }
	      else return config
			
	-- Use cabal to install base libs into a GHC build.
	when (configDoGhcLibs configNew)
	 $ ghcLibs configNew
			
	-- Download the latest Repa repo.
	when (configDoRepaUnpack configNew)
	 $ repaUnpack configNew
	
	-- Build Repa packages and register then with the current compiler.
	when (configDoRepaBuild configNew)
	 $ repaBuild configNew
		
	-- Test Repa and write results to file, or mail them to the list.
	when (configDoRepaTest configNew)
	 $ repaTest configNew env

