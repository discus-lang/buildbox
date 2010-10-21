{-# LANGUAGE PatternGuards #-}

-- | Dealing with aspects of timing results.
module BuildBox.Benchmark.TimeAspect
	( TimeAspect(..)
	, takeTimeAspectOfBenchRunResult
	, takeAvgTimeOfBenchResult
	, takeMinTimeOfBenchResult
	, takeMaxTimeOfBenchResult
	, takeMinAvgMaxOfBenchResult)
where
import BuildBox.Benchmark.Base
import BuildBox.Pretty
import Control.Monad


-- | Aspects of a benchmark runtime we can talk about.
data TimeAspect
	= TimeAspectElapsed
	| TimeAspectKernelElapsed
	| TimeAspectKernelCpu
	| TimeAspectKernelSys
	deriving (Show, Read, Enum)


-- | Get the pretty name of a TimeAspect.
instance Pretty TimeAspect where
 ppr aspect
  = case aspect of
	TimeAspectElapsed		-> ppr "elapsed"
	TimeAspectKernelElapsed		-> ppr "k.elapsed"
	TimeAspectKernelCpu		-> ppr "k.cpu"
	TimeAspectKernelSys		-> ppr "k.system"


-- | Get a particular aspect of a benchmark result.
takeTimeAspectOfBenchRunResult :: TimeAspect -> BenchRunResult -> Maybe Float
takeTimeAspectOfBenchRunResult aspect result
 = case aspect of
	TimeAspectElapsed	-> Just $ benchRunResultElapsed result
	TimeAspectKernelElapsed	-> join $ liftM timingElapsed $ benchRunResultKernel result
	TimeAspectKernelCpu	-> join $ liftM timingCpu     $ benchRunResultKernel result
	TimeAspectKernelSys	-> join $ liftM timingSys     $ benchRunResultKernel result


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
	| Just tmin	<- takeMinTimeOfBenchResult aspect result
	, Just tavg	<- takeAvgTimeOfBenchResult aspect result
	, Just tmax	<- takeMaxTimeOfBenchResult aspect result
	= Just (tmin, tavg, tmax)
	
	| otherwise
	= Nothing

