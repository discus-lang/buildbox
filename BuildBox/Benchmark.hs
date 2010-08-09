{-# LANGUAGE PatternGuards #-}

-- | Defines benchmarks that we can run.
module BuildBox.Benchmark
	( Benchmark(..)
	, Timings
	
	, BenchRunResult(..)
	, TimeAspect(..)
	, takeTimeAspectOfBenchRunResult
	
	, BenchResult(..)
	, takeAvgTimeOfBenchResult
	, takeMinTimeOfBenchResult
	, takeMaxTimeOfBenchResult
	, takeMinAvgMaxOfBenchResult
	
	, runTimedCommand
	, outRunBenchmarkSingle
	, runBenchmarkSingle
	, outRunBenchmarkSeveral)
where
import BuildBox.Build	
import BuildBox.Pretty
import Data.Time
import Control.Monad

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
	deriving (Show, Enum)


-- | Get the pretty name of a TimeAspect.
pprTimeAspect :: TimeAspect -> String
pprTimeAspect aspect
 = case aspect of
	TimeAspectElapsed		-> "elapsed"
	TimeAspectKernelElapsed		-> "kernel elapsed"
	TimeAspectKernelCpu		-> "kernel cpu"
	TimeAspectKernelSys		-> "kernel system"


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
takeAvgTimeOfBenchResult :: TimeAspect -> BenchResult -> Maybe Float
takeAvgTimeOfBenchResult aspect result
 = let	mTimes	= sequence 
		$ map (takeTimeAspectOfBenchRunResult aspect)
		$ benchResultRuns result
		
   in	liftM (\ts -> sum ts / (fromIntegral $ length ts)) mTimes
	

-- | Get the minimum runtime from a benchmark result.
takeMinTimeOfBenchResult :: TimeAspect -> BenchResult -> Maybe Float
takeMinTimeOfBenchResult aspect result
 = let	mTimes	= sequence
		$ map (takeTimeAspectOfBenchRunResult aspect)
		$ benchResultRuns result

   in	liftM (\ts -> minimum ts) mTimes


-- | Get the maximum runtime from a benchmark result.
takeMaxTimeOfBenchResult :: TimeAspect -> BenchResult -> Maybe Float
takeMaxTimeOfBenchResult aspect result
 = let	mTimes	= sequence
		$ map (takeTimeAspectOfBenchRunResult aspect)
		$ benchResultRuns result

   in	liftM (\ts -> maximum ts) mTimes


-- | Get the min, avg, and max runtimes from this benchmark result.
takeMinAvgMaxOfBenchResult :: TimeAspect -> BenchResult -> Maybe (Float, Float, Float)
takeMinAvgMaxOfBenchResult aspect result
	| Just min	<- takeMinTimeOfBenchResult aspect result
	, Just avg	<- takeAvgTimeOfBenchResult aspect result
	, Just max	<- takeMaxTimeOfBenchResult aspect result
	= Just (min, avg, max)
	
	| otherwise
	= Nothing
	


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


