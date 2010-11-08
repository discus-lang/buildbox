{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, RankNTypes #-}
module BuildBox.Benchmark.BenchResult
	( BenchResult (..)
	, mapBenchRunResult
	, liftBenchRunResult
	, statsOfBenchResult

	, BenchRunResult (..)
	, mapRunResultAspects
	, liftRunResultAspects
	, lift2RunResultAspects
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
 	= statsOfBenchResultList .  (liftBenchRunResult collateWithUnits)


statsOfBenchResultList 	:: BenchResult [] -> BenchResult Stats
statsOfBenchResultList 
	= (mapBenchRunResult . mapRunResultAspects) (applyWithUnits makeAspectStats)


-- | Transform all the `BenchRunResult`s in a `BenchResult`, perhaps changing the carrier type.
mapBenchRunResult
	:: (BenchRunResult carrier1 -> BenchRunResult carrier2) 
	-> BenchResult carrier1 -> BenchResult carrier2

mapBenchRunResult f (BenchResult name runs)
	= BenchResult name (map f runs)


-- | Apply a function to groups of aspects with the same unit.
--   TODO: dump this one.
liftToBenchResult
	:: ([WithUnits (Aspect carrier1)] -> [WithUnits (Aspect carrier2)])
	-> BenchResult carrier1 -> BenchResult carrier2
	
liftBenchRunResult f (BenchResult name runs)
	= BenchResult name (map (liftRunResultAspects f) runs)


-- | Apply an arity-2 function to groups of aspects with the same unit.
--   If the two BenchResults don't have the same name then `error`.
lift2BenchRunResult 
	:: ([WithUnits (Aspect c1)] -> [WithUnits (Aspect c1)] -> [WithUnits (Aspect c2)])
	-> BenchResult c1 -> BenchResult c1 -> BenchResult c2
	
lift2BenchRunResult f (BenchResult name1 runs1) (BenchResult name2 runs2)
	| name1 /= name2	
	= error $ unlines 
		[ "lift2BenchRunResult: results don't have same name"
		, "    name1 = " ++ name1
		, "    name2 = " ++ name2 ]
		
naming 

ALMOST: (zipWith . lift2RunResultAspects . lift2WithUnits) (zipWith compareAspects)

TODO: Just apply regular map and zip functions for these types.
      We shouldn't be exposing WithUnits here
      Functions should be just 
 	(BenchRunResult -> BenchRunResult) -> BenchResult -> BenchResult



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


-- | Transfrom all the aspects in `BenchRunResult`, perhaps changing the carrier type.
mapRunResultAspects 
	:: (WithUnits (Aspect carrier1) -> WithUnits (Aspect carrier2))
	-> BenchRunResult carrier1 -> BenchRunResult carrier2

mapRunResultAspects f (BenchRunResult ix aspects)
	= BenchRunResult ix (map f aspects)


-- | Apply a function to groups of aspects with the same unit.
liftRunResultAspects 
	:: ([WithUnits (Aspect carrier1)] -> [WithUnits (Aspect carrier2)])
	-> BenchRunResult carrier1 -> BenchRunResult carrier2

liftRunResultAspects f (BenchRunResult ix aspects)
	= BenchRunResult ix (f aspects)


-- | Apply an arity-2 function to groups of aspects with the same unit.
--   The run result index for the result it taken from the second parameter.
lift2RunResultAspects 
	:: ([WithUnits (Aspect c1)] -> [WithUnits (Aspect c1)] -> [WithUnits (Aspect c2)])
	-> BenchRunResult c1 -> BenchRunResult c1 -> BenchRunResult c2
	
lift2RunResultAspects f (BenchRunResult ix1 as1) (BenchRunResult ix2 as2)
	= BenchRunResult ix1 (f as1 as2)
