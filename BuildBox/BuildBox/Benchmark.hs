
module BuildBox.Benchmark
	( module BuildBox.Benchmark.BenchResult
	, module BuildBox.Benchmark.Benchmark
	, runTimedCommand
	, runBenchmarkOnce
	, outRunBenchmarkOnce
	, outRunBenchmarkAgainst
	, outRunBenchmarkWith)
where
import BuildBox.Build	
import BuildBox.Aspect
import BuildBox.Benchmark.Benchmark
import BuildBox.Benchmark.BenchResult
import Data.Time
import Data.List

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
	:: Integer		-- ^ Iteration number to tag results with.
	-> Benchmark 		-- ^ Benchmark to run.
	-> Build (BenchRunResult Single)
	
runBenchmarkOnce iteration bench
 = do	-- Run the setup command
	benchmarkSetup bench

	(diffTime, asRun)	
		<- runTimedCommand 
		$  benchmarkCommand bench
	
	asCheck	<- benchmarkCheck bench
	
	return	$ BenchRunResult
		{ benchRunResultIndex		= iteration

		-- Combine the aspects reported by the benchmark directly,
		-- also include our total runtime.
		, benchRunResultAspects		
			= Time TotalWall `secs` (fromRational $ toRational diffTime)
			: asRun ++ asCheck }
			
			
-- | Run a benchmark once, logging activity and timings to the console.
outRunBenchmarkOnce
	:: Integer 		-- ^ Iteration number to tag results with
	-> Benchmark		-- ^ Benchmark to run.
	-> Build (BenchRunResult Single)
	
outRunBenchmarkOnce iteration bench
 = do	out $ "Running " ++ benchmarkName bench ++ "..."
	result	<- runBenchmarkOnce iteration bench
	outLn "ok"
	outLn result
	outBlank	
	return result


-- | Run a benchmark several times, logging activity to the console.
--   Optionally print a comparison with a prior results.
outRunBenchmarkAgainst
	:: Int				-- ^ Number of iterations.
	-> Maybe (BenchResult Stats)	-- ^ Optional previous result for comparison.
	-> Benchmark			-- ^ Benchmark to run.
	-> Build (BenchResult Single)
	
outRunBenchmarkAgainst iterations mPrior bench  
 = do	out $ "Running " ++ benchmarkName bench ++ " " ++ show iterations ++ " times..."
	runResults	<- mapM ((flip runBenchmarkOnce) bench) $ take iterations [1..]
	outLn "ok"

	let result	= BenchResult
			{ benchResultName	= benchmarkName bench
			, benchResultRuns	= runResults }


--	outLn pprBenchResultAspectHeader

	case mPrior of
	 Nothing	-> outLn runResults
	 Just prior	-> outLn $ compareBenchResults prior (statBenchResult result)

	outBlank

	return	result
		
	
-- | Run a benchmark serveral times, logging activity to the console.
--   Also lookup prior results for comparison from the given list.
--   If there is no matching entry then run the benchmark anyway, but don't print the comparison.
outRunBenchmarkWith
	:: Int				-- ^ Number of times to run each benchmark to get averages.
	-> [BenchResult Stats]		-- ^ List of prior results.
	-> Benchmark			-- ^ The benchmark to run.
	-> Build (BenchResult Single)

outRunBenchmarkWith iterations priors bench
 = let	mPrior	= find (\b -> benchResultName b == benchmarkName bench) priors
   in	outRunBenchmarkAgainst iterations mPrior bench

