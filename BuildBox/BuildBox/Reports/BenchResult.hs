
module BuildBox.Reports.BenchResult
	(reportBenchResults)
where
import BuildBox.Pretty
import BuildBox.BenchMark

-- Total number of tests	
--	let totalTests	= length benchResults


-- | Produce a report of benchmark results.
reportBenchResults :: Maybe Double -> [BenchResult StatComparison] -> Doc

-- no swing specified, just report all the results.
reportBenchResults Nothing _
	= vcat $ punctuate (text "\n") $ map ppr resultComparisons
	
reportBenchResults (Just swing) comparisons
 = let	resultWinners
	 = filter 	(predBenchResult (predSwingStatsComparison (\x -> x > swing))) 
			comparisons

	resultLosers
	 = filter	(predBenchResult (predSwingStatsComparison (\x -> x < (- swing))))
			comparisons

   in	reportBenchResults' (Just swing) comparisons resultWinners resultLosers

reportBenchResults' (Just swing) comparisons resultWinners resultLosers
 	| []	<- resultsWinners
	, []	<- resultLosers
	= text "ALL GOOD"
	
	| otherwise
	= let	docWinners 
			| []	<- resultWinners
			= blank
				
			| otherwise
			= text "WINNERS:" $$ (vcat $ punctuate (text "\n") $ map ppr resultWinners)
				
		docLosers
			| []	<- resultLosers
			= blank
				
			| otherwise
			= text "LOSERS:"  $$ (vcat $ punctuate (text "\n") $ map ppr resultLosers)
		
	  in	vcat [docWinners, docLosers]
		
