
-- | Defines benchmarks that we can run.
module BuildBox.Benchmark
	( Benchmark(..)
	, runTimedCommand
	, runBenchmarkSingle)
where
import BuildBox.Build	
import Data.Time

-- Benchmark --------------------------------------------------------------------------------------
-- | Describes a benchmark that we can run.
data Benchmark a
	= Benchmark
	{ -- ^ Our internal name for the benchmark.
	  benchmarkName		:: String

	  -- ^ Setup command to run before the main benchmark.
	  --   Returns true if real benchmark is ok to run.
	, benchmarkSetup	:: Build Bool

	  -- ^ The real benchmark to run.
	, benchmarkCommand	:: Build a

	  -- ^ Check command to see if the benchmark produced the correct answer.
	, benchmarkCheck	:: Build Bool
	
	  -- ^ Optional function to parse the stdout of the benchmark to get the
	  --   elapsed,cpu,sys times for the computation kernel.
	, benchmarkParseTimes 	:: Maybe (String ->	( Maybe NominalDiffTime
				   			, Maybe NominalDiffTime
							, Maybe NominalDiffTime))
	}


-- BenchRunResult ---------------------------------------------------------------------------------
-- | The result of running a benchmark a single time.
data BenchRunResult a
	= BenchRunResult

	{ -- | The value returned by the benchmarking computation.
	  benchRunResultOutput		:: a

	  -- | The wall-clock time it took to run the benchmark.
	, benchRunResultElapsed		:: NominalDiffTime

	  -- | Elapsed time reported by the benchmark to run its kernel.
	, benchRunResultKernelElapsed	:: Maybe NominalDiffTime

	  -- | CPU time reported by the benchmark to run its kernel.
	, benchRunResultKernelCpuTime	:: Maybe NominalDiffTime

	  -- | System time reported by the benchmark to run its kernel.
	, benchRunResultKernelSysTime	:: Maybe NominalDiffTime }

-- | Aspects of a benchmark runtime we can talk about.
data TimeAspect
	= TimeAspectElapsed
	| TimeAspectKernelElapsed
	| TimeAspectKernelCpu
	| TimeAspectKernelSys

-- | Get a particular aspect of a benchmark's runtime.
takeTimeAspectOfBenchRunResult :: TimeAspect -> BenchRunResult a -> Maybe NominalDiffTime
takeTimeAspectOfBenchRunResult aspect result
 = case aspect of
	TimeAspectElapsed	-> Just $ benchRunResultElapsed result
	TimeAspectKernelElapsed	-> benchRunResultKernelElapsed result
	TimeAspectKernelCpu	-> benchRunResultKernelCpuTime result
	TimeAspectKernelSys	-> benchRunResultKernelSysTime result
	
	
-- BenchResult ------------------------------------------------------------------------------------
-- | The result of running a benchmark several times.
data BenchResult a
	= BenchResult
	{ benchResultRuns	:: [BenchRunResult a] }

-- | Get the average runtime from a benchmark result.
-- avgTimeOfBenchResult :: TimeAspect -> BenchResult -> DiffTime


-- | Get the minimum runtime from a benchmark result.
-- minTimeOfBenchResult :: BenchResult -> DiffTime

-- | Get the maximum runtime from a benchmark result.
-- maxTimeOfBenchResult :: BenchResult -> DiffTime


-- Running Commands -------------------------------------------------------------------------------
runTimedCommand 
	:: Build a
	-> Build (NominalDiffTime, a) 
		
runTimedCommand cmd
 = do	start	<- io $ getCurrentTime
	result	<- cmd
	finish	<- io $ getCurrentTime
	return (diffUTCTime finish start, result)
	

-- | Run a benchmark a single time.
runBenchmarkSingle
	:: Benchmark a
	-> Build (BenchRunResult a)
	
runBenchmarkSingle bench
 = do	out $ "Running " ++ benchmarkName bench ++ "..."

	-- Run the setup command
	_setupOk <- benchmarkSetup bench

	(diffTime, result)	
		<- runTimedCommand 
		$  benchmarkCommand bench
	
	outLn "ok"
	outLn $ "    elapsed = " ++ show diffTime
	outBlank
	
	return	$ BenchRunResult
		{ benchRunResultOutput		= result
		, benchRunResultElapsed		= diffTime
		, benchRunResultKernelElapsed	= Nothing
		, benchRunResultKernelCpuTime	= Nothing
		, benchRunResultKernelSysTime	= Nothing }

