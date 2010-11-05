{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module BuildBox.Benchmark.BenchResult
	( BenchResult (..)
	, liftToBenchRunResult
	, liftsToBenchRunResult
	, statsOfBenchResult

	, BenchRunResult (..)
	, liftToRunResultAspects
	, liftsToRunResultAspects
	, statsOfBenchResultList)
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

deriving instance ( Show (carrier Seconds), Show (carrier Bytes)) 
	         => Show (BenchResult carrier)

deriving instance ( HasUnits (carrier Bytes) Bytes
		  , Read (carrier Bytes)
                  , HasUnits (carrier Seconds) Seconds
		  , Read (carrier Seconds))
		 => Read (BenchResult carrier)

instance  ( Pretty (carrier Seconds), Pretty (carrier Bytes))
	 => Pretty (BenchResult carrier) where
 ppr result
	= hang (ppr "BenchResult" <+> text (benchResultName result)) 2
	$ vcat $ map ppr $ benchResultRuns result


statsOfBenchResult	:: BenchResult Single -> BenchResult Stats
statsOfBenchResult	
 	= statsOfBenchResultList .  (liftsToBenchRunResult collateWithUnits)


statsOfBenchResultList 	:: BenchResult [] -> BenchResult Stats
statsOfBenchResultList 
	= (liftToBenchRunResult . liftToRunResultAspects) (liftWithUnits makeAspectStats)


liftToBenchRunResult 
	:: (BenchRunResult carrier1 -> BenchRunResult carrier2)
	-> BenchResult carrier1 -> BenchResult carrier2

liftToBenchRunResult f (BenchResult name runs)
	= BenchResult name (map f runs)


liftsToBenchRunResult
	:: ([WithUnits (Aspect carrier1)] -> [WithUnits (Aspect carrier2)])
	-> BenchResult carrier1 -> BenchResult carrier2
	
liftsToBenchRunResult f (BenchResult name runs)
	= BenchResult name (map (liftsToRunResultAspects f) runs)




	

-- BenchRunResult ---------------------------------------------------------------------------------
-- | The result of running a benchmark once.
data BenchRunResult carrier
	= BenchRunResult
	{ -- | What iteration this run was.
	  benchRunResultIndex	:: Integer

	  -- | Aspects of the benchmark run.
	, benchRunResultAspects	:: [WithUnits (Aspect carrier)] }

deriving instance ( Show (carrier Seconds), Show (carrier Bytes)) 
		 => Show (BenchRunResult carrier)

deriving instance ( HasUnits (carrier Bytes) Bytes
		  , Read (carrier Bytes)
		  , HasUnits (carrier Seconds) Seconds
		  , Read (carrier Seconds))
		 => Read (BenchRunResult carrier)

instance  ( Pretty (carrier Seconds), Pretty (carrier Bytes)) 
	 => Pretty (BenchRunResult carrier) where
 ppr result
	= hang (ppr "BenchRunResult" <+> ppr (benchRunResultIndex result)) 2 
	$ vcat $ map ppr $ benchRunResultAspects result


liftToRunResultAspects 
	:: (WithUnits (Aspect carrier1) -> WithUnits (Aspect carrier2))
	-> BenchRunResult carrier1 -> BenchRunResult carrier2

liftToRunResultAspects f (BenchRunResult ix aspects)
	= BenchRunResult ix (map f aspects)


liftsToRunResultAspects 
	:: ([WithUnits (Aspect carrier1)] -> [WithUnits (Aspect carrier2)])
	-> BenchRunResult carrier1 -> BenchRunResult carrier2

liftsToRunResultAspects f (BenchRunResult ix aspects)
	= BenchRunResult ix (f aspects)
	