
-- | Comparing aspects.
module BuildBox.Aspect.Comparison
	( Comparison	(..)
	, makeComparison
	
	, StatsComparison(..)
	, makeStatsComparison
	, makeStatsComparisonNew)
where
import BuildBox.Aspect.Stats
import BuildBox.Pretty
import Text.Printf


-- | Minimum, average and maximum values.
data Comparison a	
	= Comparison
	{ comparisonBaseline	:: a
	, comparisonRecent	:: a
	, comparisonSwing	:: Double }
	
	| ComparisonNew
	{ comparisonNew		:: a }
	
	deriving (Read, Show)

instance Pretty a => Pretty (Comparison a) where
	ppr (Comparison _ recent ratio)
		= text $ printf "%s (%+4.0g)"
				(render $ ppr recent)
				ratio

	ppr (ComparisonNew new)
		= (padL 10 $ ppr new)
		

-- | Make stats from a list of values.
makeComparison :: Real a => a -> a -> Comparison a
makeComparison base recent
	= Comparison base recent swing
	
	where	dBase	= fromRational $ toRational base
		dRecent	= fromRational $ toRational recent
		swing = ((dRecent - dBase) / dBase) * 100


data StatsComparison a
	= StatsComparison (Stats (Comparison a))
	deriving (Read, Show)

instance Pretty a => Pretty (StatsComparison a) where
	ppr (StatsComparison stats) = ppr stats

makeStatsComparison :: Real a => Stats a -> Stats a -> StatsComparison a

makeStatsComparison x y
	= StatsComparison (liftStats2 makeComparison x y)
	

makeStatsComparisonNew :: Stats a -> StatsComparison a
makeStatsComparisonNew x
	= StatsComparison (liftStats ComparisonNew x)