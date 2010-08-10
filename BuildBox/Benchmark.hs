{-# LANGUAGE PatternGuards #-}

-- | Running benchmarks and collecting timings. These functions expect the given `Build` commands to succeed,
--   throwing an error if they don't. If you're not sure whether your command will succeed then test it first.
module BuildBox.Benchmark
	( module BuildBox.Benchmark.TimeAspect
	, module BuildBox.Benchmark.Pretty
	, module BuildBox.Benchmark.Compare
	
	-- * Types
	, Benchmark(..)
	, Timing(..)
	, BenchRunResult(..)
	, BenchResult(..)
	
	-- * Benchmarking
	, runTimedCommand
	, runBenchmarkOnce
	, outRunBenchmarkOnce
	, outRunBenchmarkAgainst
	, outRunBenchmarkWith)
where
import BuildBox.Build	
import BuildBox.Pretty
import BuildBox.Benchmark.Base
import BuildBox.Benchmark.TimeAspect
import BuildBox.Benchmark.Pretty
import BuildBox.Benchmark.Compare
import Data.Time
import Data.List
import Control.Monad


-- Running Commands -------------------------------------------------------------------------------
-- | Run a command, returning its elapsed time.
runTimedCommand 
	:: Build a
	-> Build (NominalDiffTime, a) 
		
runTimedCommand cmd
 = do	start	<- io $ getCurrentTime
	result	<- cmd
	finish	<- io $ getCurrentTime
	return (diffUTCTime finish start, result)


-- | Run a benchmark once.
runBenchmarkOnce
	:: Benchmark 
	-> Build BenchRunResult
	
runBenchmarkOnce bench
 = do	-- Run the setup command
	_setupOk <- benchmarkSetup bench

	(diffTime, mKernelTimings)	
		<- runTimedCommand 
		$  benchmarkCommand bench
	
	return	$ BenchRunResult
		{ benchRunResultElapsed		= fromRational $ toRational diffTime
		, benchRunResultKernel		= mKernelTimings }


-- | Run a benchmark once, logging activity and timings to the console.
outRunBenchmarkOnce
	:: Benchmark
	-> Build BenchRunResult
	
outRunBenchmarkOnce bench
 = do	out $ "Running " ++ benchmarkName bench ++ "..."
	result	<- runBenchmarkOnce bench
	outLn "ok"
	outLn $ text "    elapsed        = " <> (pprFloatTime $ benchRunResultElapsed result)
		
	maybe (return ()) (\t -> outLn $ text "    kernel elapsed = " <> pprFloatTime t) 
		$ takeTimeAspectOfBenchRunResult TimeAspectKernelElapsed result

	maybe (return ()) (\t -> outLn $ text "    kernel cpu     = " <> pprFloatTime t) 
		$ takeTimeAspectOfBenchRunResult TimeAspectKernelCpu result

	maybe (return ()) (\t -> outLn $ text "    kernel system  = " <> pprFloatTime t)
		$ takeTimeAspectOfBenchRunResult TimeAspectKernelSys result
	
	outBlank
	
	return result


-- | Run a benchmark several times, logging activity to the console.
--   Optionally print a comparison with a prior results.
outRunBenchmarkAgainst
	:: Int			-- ^ Number of iterations.
	-> Maybe BenchResult	-- ^ Optional previous result for comparison.
	-> Benchmark		-- ^ Benchmark to run.
	-> Build BenchResult
	
outRunBenchmarkAgainst iterations mPrior bench  
 = do	out $ "Running " ++ benchmarkName bench ++ " " ++ show iterations ++ " times..."
	runResults	<- replicateM iterations (runBenchmarkOnce bench) 
	outLn "ok"

	let result	= BenchResult
			{ benchResultName	= benchmarkName bench
			, benchResultRuns	= runResults }

	outLn pprBenchResultAspectHeader
	
	maybe (return ()) outLn	$ pprBenchResultAspect TimeAspectElapsed	mPrior result
	maybe (return ()) outLn	$ pprBenchResultAspect TimeAspectKernelElapsed	mPrior result
	maybe (return ()) outLn	$ pprBenchResultAspect TimeAspectKernelCpu	mPrior result
	maybe (return ()) outLn	$ pprBenchResultAspect TimeAspectKernelSys	mPrior result
		
	outBlank
	return	result


-- | Run a benchmark serveral times, logging activity to the console.
--   Also lookup prior results for comparison from the given list.
--   If there is no matching entry then run the benchmark anyway, but don't print the comparison.
outRunBenchmarkWith
	:: Int			-- ^ Number of times to run each benchmark to get averages.
	-> [BenchResult]	-- ^ List of prior results.
	-> Benchmark		-- ^ The benchmark to run.
	-> Build BenchResult

outRunBenchmarkWith iterations priors bench
 = let	mPrior	= find (\b -> benchResultName b == benchmarkName bench) priors
   in	outRunBenchmarkAgainst iterations mPrior bench
	
	
