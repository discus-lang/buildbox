
-- | Comparing aspects.
module BuildBox.Aspect.Comparison
	( Comparison	(..)
	, makeComparison)
where
import BuildBox.Aspect.Single
import BuildBox.Data.Dividable
import BuildBox.Pretty

-- | Minimum, average and maximum values.
data Comparison a	
	= Comparison
	{ comparisonBaseline	:: a
	, comparisonRecent	:: a
	, comparisonRatio	:: a }
	deriving (Read, Show)

instance Pretty a => Pretty (Comparison a) where
	ppr (Comparison base recent ratio)
		=  ppr base   <> text "/" 
		<> ppr recent <> parens (ppr ratio)


-- | Make stats from a list of values.
makeComparison :: (Num a, Dividable a, Ord a) => Single a -> Single a -> Comparison a
makeComparison (Single base) (Single recent)
	= Comparison base recent (recent `divide` base)
