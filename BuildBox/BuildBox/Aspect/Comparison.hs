
module BuildBox.Aspect.Comparison
	( -- * Comparisons
	  Comparison	(..)
	, makeComparison
	, appSwing)
where
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
	

