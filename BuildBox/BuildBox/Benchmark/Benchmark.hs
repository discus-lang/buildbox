
module BuildBox.Benchmark.Benchmark
	( Benchmark(..)
	, BenchRunResult(..)
	, BenchResult(..))
where
import BuildBox.Build
import BuildBox.Pretty
import Control.Monad

-- | Describes a benchmark that we can run.
data Benchmark
	= Benchmark
	{ -- | A unique name for the benchmark.
	  benchmarkName		:: String

	  -- | Setup command to run before the main benchmark.
	, benchmarkSetup	:: Build ()

	  -- | The benchmark command to run. 
	  --   Only the time taken to run this part is measured.
	, benchmarkCommand	:: Build [Aspect]

	  -- | Check \/ cleanup command to run after the main benchmark.
	, benchmarkCheck	:: Build ()
	}


