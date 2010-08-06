
-- | Defines benchmarks that we can run.
module BuildBox.Benchmark
	( Benchmark(..)
	, Timings
	, runTimedCommand
	, runBenchmarkSingle)
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
	, benchmarkCommand	:: Build (Maybe Timings)

	  -- ^ Check command to see if the benchmark produced the correct answer.
	, benchmarkCheck	:: Build Bool
	}

type Timings
 = 	( Maybe Float
	, Maybe Float
	, Maybe Float)

-- BenchRunResult ---------------------------------------------------------------------------------
-- | The result of running a benchmark a single time.
data BenchRunResult
	= BenchRunResult

	{ -- | The wall-clock time it took to run the benchmark.
	  benchRunResultElapsed		:: Float

	  -- | Elapsed time reported by the benchmark to run its kernel.
	, benchRunResultKernelElapsed	:: Maybe Float

	  -- | CPU time reported by the benchmark to run its kernel.
	, benchRunResultKernelCpuTime	:: Maybe Float

	  -- | System time reported by the benchmark to run its kernel.
	, benchRunResultKernelSysTime	:: Maybe Float }


-- | Aspects of a benchmark runtime we can talk about.
data TimeAspect
	= TimeAspectElapsed
	| TimeAspectKernelElapsed
	| TimeAspectKernelCpu
	| TimeAspectKernelSys


-- | Get a particular aspect of a benchmark's runtime.
takeTimeAspectOfBenchRunResult :: TimeAspect -> BenchRunResult -> Maybe Float
takeTimeAspectOfBenchRunResult aspect result
 = case aspect of
	TimeAspectElapsed	-> Just $ benchRunResultElapsed result
	TimeAspectKernelElapsed	-> benchRunResultKernelElapsed result
	TimeAspectKernelCpu	-> benchRunResultKernelCpuTime result
	TimeAspectKernelSys	-> benchRunResultKernelSysTime result
	
	
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
	return (diffUTCTime finish start, result)
	

-- | Run a benchmark a single time.
runBenchmarkSingle
	:: Benchmark
	-> Build BenchRunResult
	
runBenchmarkSingle bench
 = do	out $ "Running " ++ benchmarkName bench ++ "..."

	-- Run the setup command
	_setupOk <- benchmarkSetup bench

	(diffTime, mKernelTimings)	
		<- runTimedCommand 
		$  benchmarkCommand bench
	
	outLn "ok"
	outLn $ "    elapsed        = " ++ show diffTime
		
	(case mKernelTimings of
	 Nothing	-> return ()
	 Just (mElapsed, mCpu, mSystem)
	  -> do	maybe (return ()) (\t -> outLn $ "    kernel elapsed = " ++ show t) mElapsed
		maybe (return ()) (\t -> outLn $ "    kernel cpu     = " ++ show t) mCpu
		maybe (return ()) (\t -> outLn $ "    kernel system  = " ++ show t) mSystem)
	
	outBlank
	
	return	$ BenchRunResult
		{ benchRunResultElapsed		= fromRational $ toRational diffTime
		, benchRunResultKernelElapsed	= Nothing
		, benchRunResultKernelCpuTime	= Nothing
		, benchRunResultKernelSysTime	= Nothing }

