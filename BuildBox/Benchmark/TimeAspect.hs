{-# LANGUAGE PatternGuards #-}

module BuildBox.Benchmark.TimeAspect
	( TimeAspect(..)
	, pprTimeAspect
	, takeTimeAspectOfBenchRunResult
	, takeAvgTimeOfBenchResult
	, takeMinTimeOfBenchResult
	, takeMaxTimeOfBenchResult
	, takeMinAvgMaxOfBenchResult)
where
import BuildBox.Benchmark.Base
import Control.Monad


-- | Aspects of a benchmark runtime we can talk about.
data TimeAspect
	= TimeAspectElapsed
	| TimeAspectKernelElapsed
	| TimeAspectKernelCpu
	| TimeAspectKernelSys
	deriving (Show, Read, Enum)


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

