
module BuildBox.Benchmark.BenchResult
	( BenchResult (..)
	, BenchRunResult (..))
where
import BuildBox.Benchmark.Aspect


-- | The result of running a benchmark several times.
--   We include the name of the original benchmark to it's easy to lookup the results.
data BenchResult
	= BenchResult
	{ benchResultName	:: String
	, benchResultRuns	:: [BenchRunResult] }
	deriving (Show, Read)


-- | The result of running a benchmark once.
data BenchRunResult
	= BenchRunResult
	{ -- | What number run this was.
	  benchRunResultIndex	:: Float

	  -- | Benchmarking aspects.
	, benchRunResultAspect	:: [Aspect] }
	deriving (Show, Read)



-- Pretty Printers --------------------------------------------------------------------------------
instance Pretty BenchResult where
 ppr result
	= hang (ppr "BenchResult") 2 $ vcat
	[ ppr $ benchResultName result
	, vcat $ map ppr $ benchResultRuns result ]


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
