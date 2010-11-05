{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module BuildBox.Benchmark.BenchResult
	( BenchResult (..)
	, BenchRunResult (..))
where
import BuildBox.Aspect
import BuildBox.Pretty
import Control.Monad

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

