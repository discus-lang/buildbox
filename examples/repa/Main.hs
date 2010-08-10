{-# LANGUAGE PatternGuards #-}

import BuildBox
import Args
import Config
import Benchmarks
import Control.Monad
import System.Console.ParseArgs
import Data.Time
import Data.List

main 
 = do	args		<- parseArgsIO ArgsComplete buildArgs

	case getArg args ArgDoDump of
	 Just fileName
	  -> do	contents	<- readFile fileName
		let results	=  (read contents) :: BuildResults
		putStrLn $ render $ ppr results
		
	 Nothing 
	  -> do	let Just tmpDir		= getArg args ArgTmpDir
		tmpDir `seq` return ()

		let Just iterations	= getArg args ArgTestIterations
	
		let config
			= Config
			{ configVerbose		= gotArg args ArgVerbose
			, configTmpDir		= tmpDir
			, configDoBuild		= gotArg args ArgDoBuild
			, configDoTest		= gotArg args ArgDoTest 
			, configIterations	= iterations 
			, configWriteResults	= getArg args ArgWriteResults
			, configWithResults	= getArg args ArgWithResults }

		result	<- runBuildAndPrintResult (build config)
		return ()

	
-- | Run the complete nightly build.
build config
 = do	outLine
	outLn "Repa BuildBot\n"
	
	env	<- getEnvironmentWith 
			[ ("GHC", getVersionGHC)
			, ("GCC", getVersionGCC) ]
			
	outLn $ render $ ppr $ env
	
	outLine
	outBlank
	
	when (configDoBuild config)
	 $ buildRepaIn (configTmpDir config)
		
	when (configDoTest config)
	 $ testRepa config env
	

-- Building ---------------------------------------------------------------------------------------	
-- | Download the Repa package from code.haskell.org,
--   build it and register with current compiler.
buildRepaIn scratchDir
 = do	outCheckOk "* Checking Google is reachable"
	 $ HostReachable "www.google.com"

	outCheckOk "* Checking code.haskell.org is reachable"
	 $ HostReachable "code.haskell.org"
	
	outCheckOk "* Checking code.haskell.org web server is up"
	 $ UrlGettable "http://code.haskell.org"
	
	out "\n"
	inDir scratchDir
	 $ do	outLn "* Getting Darcs Package"
		system "darcs get http://code.haskell.org/repa/repa-head"
		
		outLn "\n"
		inDir "repa-head"
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
testRepa :: Config -> Environment -> Build ()
testRepa config env
 = do	
	-- Get the current time.
	utcTime	<- io $ getCurrentTime

	-- Load the baseline file if it was given.
	mBaseline <- case configWithResults config of
			Nothing		-> return Nothing
			Just fileName
			 -> do	file	<- io $ readFile fileName
				return	$ Just file

	-- Run the benchmarks in the build directory
	benchResults
	 <- inDir (configTmpDir config ++ "/repa-head")
 	 $ do	mapM 	(outRunBenchmarkAgainst 
				(configIterations config) 
				(liftM (buildResultBench . read) mBaseline) )
			(benchmarks config)

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
	
	-- Print the complete set of results.	
	outLine
	outLn $ render $ ppr buildResults
		

