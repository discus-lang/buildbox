
-- | Defines benchmarks that we can run.
module BuildBox.Benchmark
	(Benchmark(..))
where
import BuildBox.Build	
import Data.Time

-- Benchmark --------------------------------------------------------------------------------------
-- | Describes a benchmark that we can run.
data Benchmark
	= Benchmark
	{ -- ^ Our internal name for the benchmark.
	  benchmarkName		:: String

	  -- ^ Setup command to run before the main benchmark.
	  --   Returns true if real benchmark is ok to run.
	, benchmarkSetup	:: Build Bool

	  -- ^ The real benchmark to run.
	, benchmarkCommand	:: Build ()

	  -- ^ Check command to see if the benchmark produced the correct answer.
	, benchmarkCheck	:: Build Bool }


-- BenchRunResult ---------------------------------------------------------------------------------
-- | The result of running a benchmark a single time.
data BenchRunResult
	= BenchRunResult
	{ benchRunResultElapsed		:: DiffTime
	, benchRunResultCpuTime		:: Maybe DiffTime
	, benchRunResultSysTime		:: Maybe DiffTime }

-- | Aspects of a benchmark runtime we can talk about.
data TimeAspect
	= TimeAspectElapsed
	| TimeAspectCpu
	| TimeAspectSys

-- | Get a particular aspect of a benchmark's runtime.
takeTimeAspectOfBenchRunResult :: TimeAspect -> BenchRunResult -> Maybe DiffTime
takeTimeAspectOfBenchRunResult aspect result
 = case aspect of
	TimeAspectElapsed	-> Just $ benchRunResultElapsed result
	TimeAspectCpu		-> benchRunResultCpuTime result
	TimeAspectSys		-> benchRunResultSysTime result
	
	
-- BenchResult ------------------------------------------------------------------------------------
-- | The result of running a benchmark several times.
data BenchResult
	= BenchResult
	{ benchResultRuns	:: [BenchRunResult] }

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
	return (diffUTCTime start finish, result)
	
	

