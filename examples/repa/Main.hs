
import BuildBox

import Args
import Control.Monad
import System.Console.ParseArgs
import Data.Time

-- | Buildbot command line configuration.
data Config
	= Config
	{ configVerbose		:: Bool
	, configTmpDir		:: String
	, configDoBuild		:: Bool
	, configDoTest		:: Bool 
	, configIterations	:: Int
	, configWriteResults	:: Maybe FilePath }
	deriving Show

main 
 = do	args		<- parseArgsIO ArgsComplete buildArgs

	let Just tmpDir		= getArg args ArgTmpDir
	tmpDir `seq` return ()

	let Just iterations	= getArg args ArgTestIterations
	iterations `seq` return ()

	let config
		= Config
		{ configVerbose		= gotArg args ArgVerbose
		, configTmpDir		= tmpDir
		, configDoBuild		= gotArg args ArgDoBuild
		, configDoTest		= gotArg args ArgDoTest 
		, configIterations	= iterations 
		, configWriteResults	= getArg args ArgWriteResults }

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

	
testRepa :: Config -> Environment -> Build ()
testRepa config env
 = do	
	utcTime	<- io $ getCurrentTime
	benchResults
	 <- inDir (configTmpDir config ++ "/repa-head")
 	 $ do	outLn	$ show $ configWriteResults config
		mapM 	(outRunBenchmarkSeveral (configIterations config)) 
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
		

benchmarks config
 = let	systemWithTimings' = systemWithTimings (configVerbose config)
   in	
	-- mmult
	[ let	mmult 	= "repa-examples/dist/build/repa-mmult/repa-mmult"
	  in	Benchmark
			"mmult"
			(test $ HasExecutable mmult)
			(systemWithTimings' $ mmult ++ " -random 1024 1024 -random 1024 1024 +RTS -N4 -qg")
			(return True)
	
	-- laplace
	, let	laplace = "repa-examples/dist/build/repa-laplace/repa-laplace"
		input	= "repa-examples/Laplace/data/pls-400x400.bmp"
		inputgz	= input ++ ".gz"

	  in  	Benchmark
		 	"laplace"
			(do	makeDirIfNeeded "output"
				test $ HasExecutable laplace
				whenM (test $ HasFile inputgz)
				 $ system $ "gzip -d " ++ inputgz
				test $ HasFile input)								

			(systemWithTimings' $ laplace ++ " 1000 " ++ input ++ " output/laplace.bmp +RTS -N4 -qg")
			(return True)

	-- fft2d-highpass
	, let	fft2d	= "repa-examples/dist/build/repa-fft2d-highpass/repa-fft2d-highpass"
		input	= "repa-examples/FFT/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
	  in	Benchmark 
			"fft2d-highpass"
			(do	makeDirIfNeeded "output"
				test $ HasExecutable fft2d
				whenM (test $ HasFile inputgz)
				 $ system $ "gzip -d " ++ inputgz
				test $ HasFile input)

			(systemWithTimings' $ fft2d ++ " 1 " ++ input ++ " output/fft2d.bmp +RTS -N4 -qg")
			(return True)

	-- fft3d-highpass
	, let	fft3d	= "repa-examples/dist/build/repa-fft3d-highpass/repa-fft3d-highpass"
	  in	Benchmark
			"fft3d-highpass"
			(do	makeDirIfNeeded "output/fft3d"
				test $ HasExecutable fft3d)
			(systemWithTimings' $ fft3d ++ " 128 " ++ " output/fft3d/slice +RTS -N4 -qg")
			(return True)			
	]


systemWithTimings :: Bool -> String -> Build (Maybe Timings)
systemWithTimings verbose cmd
 = do	when verbose
	 $ outLn $ "\n    " ++ cmd
	result	<- systemWithStdout cmd
	return	$ Just $ parseTimings result


parseTimings :: String -> Timings
parseTimings str
 = let	(lElapsed : _)	= lines str
	elapsedTime	= tail $ dropWhile (/= '=') lElapsed
   in	( Just $ (read elapsedTime) / 1000
	, Nothing
	, Nothing)

