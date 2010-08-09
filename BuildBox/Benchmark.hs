{-# LANGUAGE PatternGuards #-}

-- | Defines benchmarks that we can run.
module BuildBox.Benchmark
	( module BuildBox.Benchmark.Base
	, module BuildBox.Benchmark.TimeAspect
	
	, runTimedCommand
	, outRunBenchmarkSingle
	, runBenchmarkSingle
	, outRunBenchmarkSeveral)
where
import BuildBox.Build	
import BuildBox.Pretty
import BuildBox.Benchmark.Base
import BuildBox.Benchmark.TimeAspect
import Data.Time
import Control.Monad


-- Running Commands -------------------------------------------------------------------------------
runTimedCommand 
	:: Build a
	-> Build (NominalDiffTime, a) 
		
runTimedCommand cmd
 = do	start	<- io $ getCurrentTime
	result	<- cmd
	finish	<- io $ getCurrentTime
	return (diffUTCTime finish start, result)
	

-- | Run a benchmark a single time, printing results.
outRunBenchmarkSingle
	:: Benchmark
	-> Build BenchRunResult
	
outRunBenchmarkSingle bench
 = do	out $ "Running " ++ benchmarkName bench ++ "..."
	result	<- runBenchmarkSingle bench
	outLn "ok"
	outLn $ "    elapsed        = " ++ (pprFloatTime $ benchRunResultElapsed result)
		
	maybe (return ()) (\t -> outLn $ "    kernel elapsed = " ++ pprFloatTime t) 
		$ benchRunResultKernelElapsed result

	maybe (return ()) (\t -> outLn $ "    kernel cpu     = " ++ pprFloatTime t) 
		$ benchRunResultKernelCpuTime result

	maybe (return ()) (\t -> outLn $ "    kernel system  = " ++ pprFloatTime t)
		$ benchRunResultKernelSysTime result
	
	outBlank
	
	return result
	
	
-- | Run a benchmark a single time.
runBenchmarkSingle
	:: Benchmark 
	-> Build BenchRunResult
	
runBenchmarkSingle bench
 = do	-- Run the setup command
	_setupOk <- benchmarkSetup bench

	(diffTime, mKernelTimings)	
		<- runTimedCommand 
		$  benchmarkCommand bench
	
	case mKernelTimings of
	 Nothing 
	  -> return	
		$ BenchRunResult
		{ benchRunResultElapsed		= fromRational $ toRational diffTime
		, benchRunResultKernelElapsed	= Nothing
		, benchRunResultKernelCpuTime	= Nothing
		, benchRunResultKernelSysTime	= Nothing }

	 Just (mElapsed, mCpu, mSystem) 
	  -> return	
		$ BenchRunResult
		{ benchRunResultElapsed		= fromRational $ toRational diffTime
		, benchRunResultKernelElapsed	= mElapsed
		, benchRunResultKernelCpuTime	= mCpu
		, benchRunResultKernelSysTime	= mSystem }


-- | Run a benchmark several times
outRunBenchmarkSeveral
	:: Int
	-> Benchmark
	-> Build BenchResult
	
outRunBenchmarkSeveral iterations bench
 = do	out $ "Running " ++ benchmarkName bench ++ " " ++ show iterations ++ " times..."
	runResults	<- replicateM iterations (runBenchmarkSingle bench) 
	outLn "ok"

	let result	= BenchResult
			{ benchResultRuns	= runResults }

	outLn pprBenchResultAspectHeader
	
	maybe (return ()) outLn	$ pprBenchResultAspect TimeAspectElapsed	result
	maybe (return ()) outLn	$ pprBenchResultAspect TimeAspectKernelElapsed	result
	maybe (return ()) outLn	$ pprBenchResultAspect TimeAspectKernelCpu	result
	maybe (return ()) outLn	$ pprBenchResultAspect TimeAspectKernelSys	result
		
	
	outBlank
	return	result


pprBenchResultAspectHeader :: String
pprBenchResultAspectHeader 
	=  "               "
	++ "    "
	++ "     min"
	++ "     avg"
	++ "     max"

pprBenchResultAspect :: TimeAspect -> BenchResult -> Maybe String
pprBenchResultAspect aspect result
 	| Just (min, avg, max)	<- takeMinAvgMaxOfBenchResult aspect result
	= Just
	$	"    "
		++ padR 15 (pprTimeAspect aspect)
		++ "    "
		++ (padR 7 $ pprFloatTime min)
		++ " "
		++ (padR 7 $ pprFloatTime avg)
		++ " "
		++ (padR 7 $ pprFloatTime max)
	
	| otherwise
	= Nothing	


