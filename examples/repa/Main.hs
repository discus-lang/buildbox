{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

import BuildBox
import Args
import Config
import Repa
import Control.Monad
import System.Console.ParseArgs
import Data.Time
import Data.List
import Data.Maybe

main 
 = do	args	<- parseArgsIO ArgsTrailing buildArgs
	mainWithArgs args

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
		
	-- Building and testing.
	| or $ map (gotArg args) [ArgDoNightly, ArgDoUnpack, ArgDoBuild, ArgDoTest]
	, tmpDir		<- fromMaybe 	(error "You must specify --dir with --unpack, --build or --test.")
						(getArg args ArgTmpDir)
	= tmpDir `seq` do
		let Just iterations	= getArg args ArgTestIterations
		let config
			= Config
			{ configVerbose		= gotArg args ArgVerbose
			, configTmpDir		= tmpDir
			, configWithGhcBuild	= getArg args ArgWithGhcBuild

 			, configWithGhc 	= maybe "ghc" (\dir -> dir ++ "/inplace/bin/ghc-stage2") 
						$ getArg args ArgWithGhcBuild

			, configWithGhcPkg	= maybe "ghc-pkg" (\dir -> dir ++ "/inplace/bin/ghc-pkg") 
						$ getArg args ArgWithGhcBuild

			, configDoUnpack	= gotArg args ArgDoUnpack || gotArg args ArgDoNightly
			, configDoBuild		= gotArg args ArgDoBuild  || gotArg args ArgDoNightly
			, configDoTest		= gotArg args ArgDoTest   || gotArg args ArgDoNightly
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

		result	<- runBuildAndPrintResult (nightly config)
		return ()

	| otherwise
	= usageError args "You must specify at least one of --dump --build or --test.\n"


-- Nightly ----------------------------------------------------------------------------------------
-- | Run the complete nightly build.
nightly config
 = do	outLine
	outLn "Repa BuildBot\n"
	
	env	<- getEnvironmentWith 
			[ ("GHC", getVersionGHC $ configWithGhc config)
			, ("GCC", getVersionGCC "gcc") ]
			
	outLn $ render $ ppr $ env
	
	outLine
	outBlank
	
	when (configDoUnpack config)
	 $ repaUnpack config
	
	when (configDoBuild config)
	 $ repaBuild config
		
	when (configDoTest config)
	 $ repaTest config env



