{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

import BuildBox
import Args
import Config
import Benchmarks
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

		putStrLn $ render $ pprComparisons baseline current
		
	-- Building and testing.
	|   gotArg args ArgDoUnpack
	 || gotArg args ArgDoBuild 
	 || gotArg args ArgDoTest
	, tmpDir		<- fromMaybe 	(error "You must specify --dir with --unpack, --build or --test.")
						(getArg args ArgTmpDir)
	= tmpDir `seq` do
		let Just iterations	= getArg args ArgTestIterations
		let config
			= Config
			{ configVerbose		= gotArg args ArgVerbose
			, configTmpDir		= tmpDir
			, configDoUnpack	= gotArg args ArgDoUnpack
			, configDoBuild		= gotArg args ArgDoBuild
			, configDoTest		= gotArg args ArgDoTest 
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
			[ ("GHC", getVersionGHC)
			, ("GCC", getVersionGCC) ]
			
	outLn $ render $ ppr $ env
	
	outLine
	outBlank
	
	when (configDoUnpack config)
	 $ repaUnpack config
	
	when (configDoBuild config)
	 $ repaBuild config
		
	when (configDoTest config)
	 $ repaTest config env



-- Unpack -----------------------------------------------------------------------------------------
-- | Download the Repa package from code.haskell.org,
repaUnpack config
 = do	outCheckFalseOk "* Checking build directory is empty"
	 $ HasDir $ (configTmpDir config) ++ "/repa-head"
	
	outCheckOk "* Checking Google is reachable"
	 $ HostReachable "www.google.com"

	outCheckOk "* Checking code.haskell.org is reachable"
	 $ HostReachable "code.haskell.org"
	
	outCheckOk "* Checking code.haskell.org web server is up"
	 $ UrlGettable "http://code.haskell.org"
	
	out "\n"
	inDir (configTmpDir config)
	 $ do	outLn "* Getting Darcs Package"
		system "darcs get http://code.haskell.org/repa/repa-head"
	


-- Building ---------------------------------------------------------------------------------------	
-- | Build the packages and register then with the given compiler.
repaBuild config
 = inDir (configTmpDir config)
 $ inDir "repa-head"
 $ do	outLn "* Building Packages"
	system "make"



-- Testing ----------------------------------------------------------------------------------------
data BuildResults
	= BuildResults
	{ buildResultTime		:: UTCTime
	, buildResultEnvironment	:: Environment
	, buildResultBench		:: [BenchResult] }
	deriving (Show, Read)

instance Pretty BuildResults where
 ppr results
	= hang (ppr "BuildResults") 2 $ vcat
	[ ppr "time: " <> (ppr $ buildResultTime results)
	, ppr $ buildResultEnvironment results
	, ppr ""
	, vcat 	$ punctuate (ppr "\n") 
		$ map ppr 
		$ buildResultBench results ]

-- | Run regression tests.	
repaTest :: Config -> Environment -> Build ()
repaTest config env
 = do	
	-- Get the current time.
	utcTime	<- io $ getCurrentTime

	-- Load the baseline file if it was given.
	mBaseline <- case configAgainstResults config of
			Nothing		-> return Nothing
			Just fileName
			 -> do	file	<- io $ readFile fileName
				return	$ Just file
				
	let resultsPrior
		= maybe []
			(\contents -> buildResultBench $ read contents)
			mBaseline

	-- Run the benchmarks in the build directory
	benchResults
	 <- inDir (configTmpDir config ++ "/repa-head")
 	 $ do	mapM 	(outRunBenchmarkWith (configIterations config)  resultsPrior)
			([head $ benchmarks config])

	-- Make the build results.
	let buildResults
		= BuildResults
		{ buildResultTime		= utcTime
		, buildResultEnvironment	= env
		, buildResultBench		= benchResults }

	-- Write results to a file if requested.	
	maybe 	(return ())
		(\fileName -> io $ writeFile fileName $ show buildResults)
		(configWriteResults config)
	
	-- Mail results to recipient if requested.
	maybe 	(return ())
		(\(from, to) -> do
			mail	<- createMailWithCurrentTime from to "Repa build"
				$ show buildResults
				
			sendMailWithMailer mail defaultMailer				
			return ())
		(configMailFromTo config)
		
		
			
			
