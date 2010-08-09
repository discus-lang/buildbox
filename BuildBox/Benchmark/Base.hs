
module BuildBox.Benchmark.Base
	( Benchmark(..)
	, Timings
	, BenchRunResult(..)
	, BenchResult(..))
where
import BuildBox.Build


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
	deriving (Show, Read)


-- | The result of running a benchmark several times.
data BenchResult
	= BenchResult
	{ benchResultName	:: String
	, benchResultRuns	:: [BenchRunResult] }
	deriving (Show, Read)

