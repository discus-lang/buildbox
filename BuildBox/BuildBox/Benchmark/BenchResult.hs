{-# LANGUAGE PatternGuards, StandaloneDeriving, FlexibleContexts, UndecidableInstances, RankNTypes #-}
module BuildBox.Benchmark.BenchResult
	( 
	-- * Benchmark results	
	  BenchResult (..)
	, concatBenchResult
	, collateBenchResult
	, statCollatedBenchResult
	, statBenchResult
	, compareBenchResults
	, compareBenchResultWith
	, compareManyBenchResults
	
	-- * Benchmark run results
	, BenchRunResult (..)

	-- * Lifting functions
	, liftBenchRunResult
	, liftBenchRunResult2
	, liftToAspectsOfBenchResult
	, liftToAspectsOfBenchResult2
	, liftRunResultAspects
	, liftRunResultAspects2)
where
import BuildBox.Aspect
import BuildBox.Pretty
import Data.List


-- BenchResult ------------------------------------------------------------------------------------
-- | The result of running a benchmark several times.
--   We include the name of the original benchmark to it's easy to lookup the results.
data BenchResult carrier
	= BenchResult
	{ benchResultName	:: String
	, benchResultRuns	:: [BenchRunResult carrier] }

deriving instance 
	(  Show (c Seconds), Show (c Bytes)) 
	=> Show (BenchResult c)

deriving instance
	(  HasUnits (c Bytes) Bytes
	,  Read (c Bytes)
	,  HasUnits (c Seconds) Seconds
	,  Read (c Seconds))
	=> Read (BenchResult c)


instance  ( Pretty (c Seconds), Pretty (c Bytes))
	 => Pretty (BenchResult c) where
 ppr result
	= text (benchResultName result)
	$+$ nest 4 (vcat $ map ppr $ benchResultRuns result)


-- | Concatenate the results of all runs.
--   In the resulting `BenchResult` has a single `BenchRunResult` with an index of 0.
--   In effect we have merged the data from all runs.
concatBenchResult :: BenchResult c1 -> BenchResult c1
concatBenchResult 
	= liftBenchRunResult 
	$ \bsResults -> [BenchRunResult 0 (concatMap benchRunResultAspects bsResults)]


-- | Collate the aspects of each run.
collateBenchResult :: BenchResult Single -> BenchResult []
collateBenchResult
	= liftToAspectsOfBenchResult collateWithUnits


-- | Compute statistics about each the aspects of each run.
--   The results need to be in collated form.
statCollatedBenchResult :: BenchResult [] -> BenchResult Stats
statCollatedBenchResult
	= liftToAspectsOfBenchResult (map (applyWithUnits makeAspectStats))


-- | Compute statistics about some the aspects of each run.
statBenchResult :: BenchResult Single -> BenchResult Stats
statBenchResult 
	= statCollatedBenchResult . collateBenchResult . concatBenchResult


-- | Compute comparisons of benchmark results.
--	Both results must have the same name else `error`
compareBenchResults
	:: BenchResult Stats -> BenchResult Stats -> BenchResult StatsComparison

compareBenchResults 
	= liftBenchRunResult2 (zipWith (liftRunResultAspects2 (liftWithUnits2 makeAspectComparisons)))


-- | Compute comparisons of benchmark result, looking up the baseline results from a given list.
--	If there are no matching baseline results then this creates a `ComparisonNew` in the output.
compareBenchResultWith 
	:: [BenchResult Stats] -> BenchResult Stats -> BenchResult StatsComparison

compareBenchResultWith base result
	| Just baseResult	<- find (\baseResult -> benchResultName baseResult == benchResultName result) base
	= compareBenchResults baseResult result
	
	| otherwise
	= liftToAspectsOfBenchResult (liftWithUnits (map (liftAspect makeStatsComparisonNew))) result


-- | Compare some baseline results against new results.
--	If there are no matching baseline results then this creates a `ComparisonNew` in the output.
compareManyBenchResults 
	:: [BenchResult Stats] -> [BenchResult Stats] -> [BenchResult StatsComparison]
	
compareManyBenchResults base new
	= map (compareBenchResultWith base) new


-- | Apply a function to the `BenchRunResult` in a `BenchResult`
liftBenchRunResult 
	:: ([BenchRunResult c1] -> [BenchRunResult  c2])
	-> (BenchResult     c1  -> BenchResult      c2)

liftBenchRunResult f (BenchResult name runs)	
	= BenchResult name (f runs)


-- | Apply a binary function to the `BenchResults` in a `BenchResult`
liftBenchRunResult2
	:: ([BenchRunResult c1] -> [BenchRunResult c2] -> [BenchRunResult c3])
	->  BenchResult c1      ->  BenchResult c2     ->  BenchResult c3

liftBenchRunResult2 f (BenchResult name1 runs1) (BenchResult name2 runs2)
	| name1 == name2	= BenchResult name1 (f runs1 runs2)
	| otherwise		= error "liftBenchRunResult2: names don't match"
	

-- | Apply a function to the aspects of each run result.
liftToAspectsOfBenchResult 
	:: ([WithUnits (Aspect c1)] -> [WithUnits (Aspect c2)])
	-> BenchResult c1           -> BenchResult c2

liftToAspectsOfBenchResult 
	= liftBenchRunResult . map . liftRunResultAspects


-- | Apply a binary function to the aspects of each run result.
liftToAspectsOfBenchResult2
	:: ([WithUnits (Aspect c1)] -> [WithUnits (Aspect c2)] -> [WithUnits (Aspect c3)])
	-> BenchResult c1           -> BenchResult c2          -> BenchResult c3

liftToAspectsOfBenchResult2
	= liftBenchRunResult2 . zipWith . liftRunResultAspects2



-- BenchRunResult ---------------------------------------------------------------------------------
-- | The result of running a benchmark once.
data BenchRunResult carrier
	= BenchRunResult
	{ -- | What iteration this run was.
	  benchRunResultIndex	:: Integer

	  -- | Aspects of the benchmark run.
	, benchRunResultAspects	:: [WithUnits (Aspect carrier)] }


deriving instance 
	(  Show (c Seconds), Show (c Bytes)) 
	=> Show (BenchRunResult c)

deriving instance
	(  HasUnits (c Bytes) Bytes
	,  Read (c Bytes)
	,  HasUnits (c Seconds) Seconds
	,  Read (c Seconds))
	=> Read (BenchRunResult c)


instance  ( Pretty (c Seconds), Pretty (c Bytes)) 
	 => Pretty (BenchRunResult c) where
 ppr result
	| benchRunResultIndex result == 0
	=  (nest 2 $ vcat $ map ppr $ benchRunResultAspects result)

	| otherwise
	= ppr (benchRunResultIndex result) 
	$$ (nest 2 $ vcat $ map ppr $ benchRunResultAspects result)

	
-- | Apply a function to the aspects on a BenchRunResult
liftRunResultAspects
	:: ([WithUnits (Aspect c1)] -> [WithUnits (Aspect c2)])
	-> BenchRunResult c1        -> BenchRunResult c2
	
liftRunResultAspects f (BenchRunResult ix as)
	= BenchRunResult ix (f as)


-- | Apply a function to the aspects on a BenchRunResult
liftRunResultAspects2
	:: ([WithUnits (Aspect c1)] -> [WithUnits (Aspect c2)] -> [WithUnits (Aspect c3)])
	-> BenchRunResult c1        -> BenchRunResult c2       -> BenchRunResult c3
	
liftRunResultAspects2 f (BenchRunResult ix1 as) (BenchRunResult ix2 bs)
	| ix1 == ix2		= BenchRunResult ix1 (f as bs)
	| otherwise		= error "liftRunResultAspects2: indices don't match"

