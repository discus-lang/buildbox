{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, RankNTypes #-}
module BuildBox.Benchmark.BenchResult
	( BenchResult (..)
	, liftBenchRunResult
	, liftToAspectsOfBenchResult
	, statBenchResults
	, statCollatedBenchResults
	, collateBenchResults
	, concatBenchResults
	
	, BenchRunResult (..)
	, liftRunResultAspects)
where
import BuildBox.Aspect
import BuildBox.Pretty

-- BenchResult ------------------------------------------------------------------------------------
-- | The result of running a benchmark several times.
--   We include the name of the original benchmark to it's easy to lookup the results.
data BenchResult carrier
	= BenchResult
	{ benchResultName	:: String
	, benchResultRuns	:: [BenchRunResult carrier] }

deriving instance 
	(  Show (carrier Seconds), Show (carrier Bytes)) 
	=> Show (BenchResult carrier)

deriving instance
	(  HasUnits (carrier Bytes) Bytes
	,  Read (carrier Bytes)
	,  HasUnits (carrier Seconds) Seconds
	,  Read (carrier Seconds))
	=> Read (BenchResult carrier)

instance  ( Pretty (carrier Seconds), Pretty (carrier Bytes))
	 => Pretty (BenchResult carrier) where
 ppr result
	= hang (ppr "BenchResult" <+> text (benchResultName result)) 2
	$ vcat $ map ppr $ benchResultRuns result


-- | Apply a function to the `BenchRunResult` in a `BenchResult`
liftBenchRunResult 
	:: ([BenchRunResult c1] -> [BenchRunResult  c2])
	-> (BenchResult     c1  -> BenchResult      c2)

liftBenchRunResult f (BenchResult name runs)	
	= BenchResult name (f runs)


-- | Apply a function to the aspects of each run result.
liftToAspectsOfBenchResult 
	:: ([WithUnits (Aspect c1)] -> [WithUnits (Aspect c2)])
	-> BenchResult c1           -> BenchResult c2

liftToAspectsOfBenchResult 
	= liftBenchRunResult . map . liftRunResultAspects


-- | Compute statistics about some the aspects of each run.
statBenchResults :: BenchResult Single -> BenchResult Stats
statBenchResults 
	= statCollatedBenchResults . collateBenchResults . concatBenchResults


-- | Compute statistics about each the aspects of each run.
--   The results need to be in collated form.
statCollatedBenchResults :: BenchResult [] -> BenchResult Stats
statCollatedBenchResults
	= liftToAspectsOfBenchResult (map (applyWithUnits makeAspectStats))
	. concatBenchResults


-- | Collate the aspects of each run.
collateBenchResults :: BenchResult Single -> BenchResult []
collateBenchResults
	= liftToAspectsOfBenchResult collateWithUnits
	

-- | Concatenate the results of all runs.
--   In the resulting `BenchResult` has a single `BenchRunResult` with an index of 0.
--   In effect we have merged the data from all runs.
concatBenchResults :: BenchResult c1 -> BenchResult c1
concatBenchResults 
	= liftBenchRunResult 
	$ \bsResults -> [BenchRunResult 0 (concatMap benchRunResultAspects bsResults)]


-- BenchRunResult ---------------------------------------------------------------------------------
-- | The result of running a benchmark once.
data BenchRunResult carrier
	= BenchRunResult
	{ -- | What iteration this run was.
	  benchRunResultIndex	:: Integer

	  -- | Aspects of the benchmark run.
	, benchRunResultAspects	:: [WithUnits (Aspect carrier)] }


deriving instance 
	(  Show (carrier Seconds), Show (carrier Bytes)) 
	=> Show (BenchRunResult carrier)

deriving instance
	(  HasUnits (carrier Bytes) Bytes
	,  Read (carrier Bytes)
	,  HasUnits (carrier Seconds) Seconds
	,  Read (carrier Seconds))
	=> Read (BenchRunResult carrier)


instance  ( Pretty (carrier Seconds), Pretty (carrier Bytes)) 
	 => Pretty (BenchRunResult carrier) where
 ppr result
	= hang (ppr "BenchRunResult" <+> ppr (benchRunResultIndex result)) 2 
	$ vcat $ map ppr $ benchRunResultAspects result

	
-- | Apply a function to the aspects on a BenchRunResult
liftRunResultAspects
	:: ([WithUnits (Aspect c1)] -> [WithUnits (Aspect c2)])
	-> BenchRunResult c1        -> BenchRunResult c2
	
liftRunResultAspects f (BenchRunResult ix as)
	= BenchRunResult ix (f as)

