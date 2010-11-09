
module BuildBox.Aspect.Comparison
	( 
	-- * Comparisons
	  Comparison	(..)
	, makeComparison
	, appSwing
	
	-- * Comparisons of Statistics
	, StatsComparison(..)
	, makeStatsComparison
	, makeStatsComparisonNew
	, predSwingStatsComparison)
where
import BuildBox.Aspect.Stats
import BuildBox.Pretty
import Text.Printf


-- | The comparison of two values.
data Comparison a	
	-- | Comparison of a recent value with a baseline.
	= Comparison
	{ comparisonBaseline	:: a
	, comparisonRecent	:: a
	, comparisonSwing	:: Double }
	
	-- | A new value that doesn't have a baseline.
	| ComparisonNew
	{ comparisonNew		:: a }
	
	deriving (Read, Show)

instance Pretty a => Pretty (Comparison a) where
	ppr (Comparison _ recent ratio)
		| abs ratio < 0.01	
		= text $ printf "%s (----)"
				(render $ ppr recent)

		| otherwise		
		= text $ printf "%s (%+4.0f)"
				(render $ ppr recent)
				(ratio * 100)

	ppr (ComparisonNew new)
		= (padL 10 $ ppr new)
		

-- | Make a comparison from two values.
makeComparison :: Real a => a -> a -> Comparison a
makeComparison base recent
	= Comparison base recent swing
	
	where	dBase	= fromRational $ toRational base
		dRecent	= fromRational $ toRational recent
		swing = ((dRecent - dBase) / dBase)


-- | Apply a function to the swing of a comparison.
appSwing :: a -> (Double -> a) -> Comparison b -> a
appSwing def f aa
 = case aa of
	Comparison _ _ swing	-> f swing
	ComparisonNew{}		-> def
	

-- StatsComparison --------------------------------------------------------------------------------
-- | Comparisons of statistics
data StatsComparison a
	= StatsComparison (Stats (Comparison a))
	deriving (Read, Show)

instance Pretty a => Pretty (StatsComparison a) where
	ppr (StatsComparison stats) = ppr stats

-- | Make a comparison of two `Stats`.
makeStatsComparison :: Real a => Stats a -> Stats a -> StatsComparison a
makeStatsComparison x y = StatsComparison (liftStats2 makeComparison x y)
	

-- | Make a `ComparisonNew`.
makeStatsComparisonNew :: Stats a -> StatsComparison a
makeStatsComparisonNew x
	= StatsComparison (liftStats ComparisonNew x)
	

-- | Return `True` if any of the swings in the `StatsComparison` match the given function.
predSwingStatsComparison :: (Double -> Bool) -> StatsComparison a -> Bool
predSwingStatsComparison f (StatsComparison ss)
	= (predStats . (appSwing False)) f ss
	