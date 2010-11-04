
module BuildBox.Benchmark.Aspect.Mode
	( Single	(..), valueOfSingle
	, Stats		(..)

	, Dividable	(..)
	, makeStats)
where
import BuildBox.Pretty
	

-- Aspect Modes -----------------------------------------------------------------------------------
-- | Use for a single valued aspect.
data Single a	
	= Single a
	deriving (Read, Show)

valueOfSingle :: Single a -> a
valueOfSingle (Single a) = a

instance Pretty a => Pretty (Single a) where
	ppr (Single x)	= ppr x


-- | Minimum, Average and Maximum values.
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


-- Dividable --------------------------------------------------------------------------------------
-- | Things that can be divided.
--   Used to make average values when building Stats.
class Dividable a where
	divide :: a -> a -> a
	
instance Dividable Integer where
	divide	= div

instance Dividable Float where
	divide	= (/)

-- | Make stats from a list of values.
makeStats :: (Num a, Dividable a, Ord a) => [a] -> Stats a
makeStats xs
	= Stats (minimum xs)
		(sum xs `divide` (fromIntegral $ length xs)) 
		(maximum xs)
