{-# LANGUAGE PatternGuards #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module BuildBox.Reports.BenchResult
	(reportBenchResults)
where
import BuildBox.Pretty
import BuildBox.Aspect
import BuildBox.Benchmark.BenchResult
import Text.Printf
import Data.List

-- Total number of tests	
--	let totalTests	= length benchResults


-- | Produce a report of benchmark results.
reportBenchResults :: Maybe Double -> [BenchResult StatsComparison] -> Doc

-- no swing specified, just report all the results.
reportBenchResults Nothing comparisons
	= vcat $ punctuate (text "\n") $ map ppr comparisons
	
reportBenchResults (Just swing) comparisons
 = let	resultLosers
	 = filter	(predBenchResult (predSwingStatsComparison (\x -> x > swing)))
			comparisons

	resultWinners_
	 = filter 	(predBenchResult (predSwingStatsComparison (\x -> x < (- swing)))) 
			comparisons

	-- losers can't be winners
	resultWinners 	= deleteFirstsBy (\r1 r2 -> benchResultName r1 == benchResultName r2)
				resultWinners_  resultLosers

   in	vcat $	[ text "Total tests = " <> int (length comparisons)
		, blank] ++ [reportBenchResults' swing resultWinners resultLosers]

reportBenchResults' swing resultWinners resultLosers
 	| []	<- resultWinners
	, []	<- resultLosers
	= text "ALL GOOD\n"
	
	| otherwise
	= let	docWinners 
			| []	<- resultWinners
			= []
				
			| otherwise
			= [text "-- WINNERS (had a swing of < "
					<> text (printf "%+2.0f" (negate (swing * 100))) <> text "%)"
			$$ (vcat $ punctuate (text "\n") $ map ppr resultWinners) 
			<> text "\n"]
				
		docLosers
			| []	<- resultLosers
			= []
				
			| otherwise
			= [text "-- LOSERS  (had a swing of > " 
					<> text (printf "%+2.0f" (swing * 100)) <> text "%)"
			$$ (vcat $ punctuate (text "\n") $ map ppr resultLosers) 
			<> text "\n"]
		
	  in	vcat $ docWinners ++ docLosers
		
