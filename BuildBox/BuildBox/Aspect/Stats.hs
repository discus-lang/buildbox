
-- | Extracting statistics from many-valued data.
module BuildBox.Aspect.Stats
	( Stats	(..)
	, makeStats)
where
import BuildBox.Pretty
import BuildBox.Data.Dividable

-- | Minimum, average and maximum values.
data Stats a	
	= Stats
	{ statsMin	:: a
	, statsAvg	:: a
	, statsMax	:: a }
	deriving (Read, Show)

instance Pretty a => Pretty (Stats a) where
	ppr (Stats mi av mx)
		=  ppr mi <> text "/" 
		<> ppr av <> text "/"
		<> ppr mx


-- | Make stats from a list of values.
makeStats :: (Num a, Dividable a, Ord a) => [a] -> Stats a
makeStats xs
	= Stats (minimum xs)
		(sum xs `divide` (fromIntegral $ length xs)) 
		(maximum xs)


	
