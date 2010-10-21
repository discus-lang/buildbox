{-# OPTIONS_HADDOCK hide #-}

module BuildBox.Benchmark.Base
	( Benchmark(..)
	, Timing(..)
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

	  -- | The benchmark command to run. Only the time taken to run this part is measured.
	, benchmarkCommand	:: Build (Maybe Timing)

	  -- | Check \/ cleanup command to run after the main benchmark.
	, benchmarkCheck	:: Build ()
	}


-- | Holds elapsed, cpu, and system timings (in seconds).
data Timing
	= Timing
	{ timingElapsed	:: Maybe Float
	, timingCpu	:: Maybe Float
	, timingSys	:: Maybe Float }
	deriving (Eq, Read, Show)


-- | The result of running a benchmark once.
data BenchRunResult
	= BenchRunResult

	{ -- | The wall-clock time taken to run the benchmark (in seconds)
	  benchRunResultElapsed		:: Float

	  -- | Time that the benchmark itself reported was taken to run its kernel.
	, benchRunResultKernel		:: Maybe Timing }

	deriving (Show, Read)


instance Pretty BenchRunResult where
 ppr result
	= hang (ppr "BenchRunResult") 2 $ vcat
	[ ppr "elapsed:        " <> (ppr $ benchRunResultElapsed result) 
	, maybe empty (\r -> ppr "k.elapsed: " <> ppr r) 
		$ join $ liftM timingElapsed $ benchRunResultKernel result

	, maybe empty (\r -> ppr "k.cpu:     " <> ppr r)
		$ join $ liftM timingCpu     $ benchRunResultKernel result

	, maybe empty (\r -> ppr "k.system:  " <> ppr r)
		$ join $ liftM timingSys     $ benchRunResultKernel result ]
	
	
-- | The result of running a benchmark several times.
--   We include the name of the original benchmark to it's easy to lookup the results.
data BenchResult
	= BenchResult
	{ benchResultName	:: String
	, benchResultRuns	:: [BenchRunResult] }
	deriving (Show, Read)

instance Pretty BenchResult where
 ppr result
	= hang (ppr "BenchResult") 2 $ vcat
	[ ppr $ benchResultName result
	, vcat $ map ppr $ benchResultRuns result ]
