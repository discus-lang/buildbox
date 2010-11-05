
module BuildBox.Benchmark.Benchmark
	( Benchmark(..)
	, BenchRunResult(..)
	, BenchResult(..))
where
import BuildBox.Build
import BuildBox.Pretty
import BuildBox.Benchmark.BenchResult
import BuildBox.Benchmark.Aspect
import BuildBox.Benchmark.Aspect.Tagged
import BuildBox.Benchmark.Aspect.Mode
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
	, benchmarkCommand	:: Build [Tagged (Aspect Single)]

	  -- | Check \/ cleanup command to run after the main benchmark.
	, benchmarkCheck	:: Build ()
	}


