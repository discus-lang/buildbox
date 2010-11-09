
module BuildBox.Benchmark.Benchmark
	(Benchmark(..))
where
import BuildBox.Build
import BuildBox.Aspect

-- | Describes a benchmark that we can run.
data Benchmark
	= Benchmark
	{ -- | A unique name for the benchmark.
	  benchmarkName		:: String

	  -- | Setup command to run before the main benchmark.
	, benchmarkSetup	:: Build ()

	  -- | The benchmark command to run. 
	  --   The time taken to run this part is automatically measured and added to the overall results.
	, benchmarkCommand	:: Build [WithUnits (Aspect Single)]

	  -- | Check \/ cleanup command to run after the main benchmark.
	, benchmarkCheck	:: Build [WithUnits (Aspect Single)]
	}


