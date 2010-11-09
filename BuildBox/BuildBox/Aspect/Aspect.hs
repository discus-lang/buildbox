{-# LANGUAGE 	ScopedTypeVariables, StandaloneDeriving,
		GADTs, FlexibleContexts, RankNTypes,
		UndecidableInstances, KindSignatures #-}
{-# OPTIONS_HADDOCK hide #-}
module BuildBox.Aspect.Aspect
	( Aspect	(..)
	, makeAspect
	, splitAspect
	, collateWithUnits
	, makeAspectStats
	, makeAspectComparison
	, makeAspectComparisons

	-- * Lifting functions
	, liftAspect
	, liftAspect2)
where
import BuildBox.Aspect.Single
import BuildBox.Aspect.Units
import BuildBox.Aspect.Detail
import BuildBox.Aspect.Stats
import BuildBox.Aspect.Comparison
import BuildBox.Pretty
import Text.Read
import Data.List
import qualified Data.Map	as Map


-- | Holds a detail about a benchmark.
--
--   The @c@ is the type constructor of the container that holds the data.
--
--   Useful instances for @c@ include `Single`, `[ ]`, `Stats`, `Comparison` and `StatsComparison`.
--
data Aspect (c :: * -> *) units where
	Time	:: Timed	-> c Seconds	-> Aspect c Seconds
	Size	:: Sized	-> c Bytes	-> Aspect c Bytes
	Used	:: Used		-> c Bytes	-> Aspect c Bytes

deriving instance Show (c units) => Show (Aspect c units)	

-- We need to write the read instance manually because it requires makeAspect
instance (  HasUnits (c units) units
	 ,  Read  (c units)) 
	 => Read (Aspect c units) where
 readPrec 
  = do	tok <- lexP
	case tok of
	 Punc  "("
	  -> do	aspect		<- readPrec
		Punc ")"	<- lexP
		return aspect
		
	 Ident "Time" 
	  -> do	timed		<- readPrec
		dat		<- readPrec
		let Just aspect	= makeAspect (DetailTimed timed) dat
		return aspect
	
	 Ident "Size"
	  -> do	sized		<- readPrec
		dat		<- readPrec
		let Just aspect	= makeAspect (DetailSized sized) dat
		return aspect

	 Ident "Used"
	  -> do	used		<- readPrec
		dat		<- readPrec
		let Just aspect	= makeAspect (DetailUsed used) dat
		return aspect
		
	 _ -> pfail


instance ( Pretty (c Seconds)
	 , Pretty (c Bytes))
 	 => Pretty (Aspect c units) where
 ppr aa
  = case aa of
	Time timed dat	-> padL 30 (ppr timed) <+> text ":" <+> ppr dat
	Size sized dat	-> padL 30 (ppr sized) <+> text ":" <+> ppr dat
	Used used  dat	-> padL 30 (ppr used)  <+> text ":" <+> ppr dat

	

-- | Split an aspect into its named detail and value.
splitAspect :: Aspect carrier units -> (Detail, carrier units)
splitAspect aa
 = case aa of
	Time timed val		-> (DetailTimed timed, val)
	Size sized val		-> (DetailSized sized, val)
	Used used  val		-> (DetailUsed  used,  val)


-- | Make an aspect from named detail and data.
--   If the detail doesn't match the units of the data then `Nothing`.
makeAspect
	:: HasUnits (c units) units 
	=> Detail -> c units -> Maybe (Aspect c units)

makeAspect detail (val :: c units)
 = case hasUnits val :: Maybe (IsUnits units) of
	Just IsSeconds
	 -> case detail of
		DetailTimed timed	-> Just (Time timed val)
		_			-> Nothing

	Just IsBytes
	 -> case detail of
		DetailUsed  used	-> Just (Used used  val)
		DetailSized sized	-> Just (Size sized val)
		_			-> Nothing

	Nothing -> Nothing
	

-- | Transform the data in an aspect, possibly changing the carrier type.
liftAspect 
	:: (c1 units -> c2 units)
	-> Aspect c1 units -> Aspect c2 units

liftAspect f aspect
 = case aspect of
	Time timed dat	-> Time timed (f dat)
	Size sized dat	-> Size sized (f dat)
	Used used dat	-> Used used  (f dat)


-- | Apply a function to the aspect data, producing a new aspect.
--   If the aspect details don't match then `error`.
liftAspect2
	:: (c1 units -> c1 units -> c2 units) 
	-> Aspect c1 units -> Aspect c1 units -> Aspect c2 units
	
liftAspect2 f a1 a2
 = case (a1, a2) of
	(Time timed1 dat1, Time timed2 dat2)
	 | timed1 == timed2	-> Time timed1 (f dat1 dat2)

	(Size sized1 dat1, Size sized2 dat2)
	 | sized1 == sized2	-> Size sized1 (f dat1 dat2)

	(Used used1 dat1,  Used used2 dat2)
	 | used1  == used2	-> Used used1 (f dat1 dat2)

	_ -> error "liftAspect2: aspects don't match"


-- Collate ----------------------------------------------------------------------------------------
instance Collatable Aspect where
 collate as
  = let	-- This Just match will always succeed provided the implementation of gather is correct.
	Just as' = sequence 
		 $ map (uncurry makeAspect) 
		 $ gather [(detail, val) | (detail, (Single val)) <- map splitAspect as]
    in	as'



-- | Gather a list of pairs on the first element
--	gather [(0, 1), (0, 2), (3, 2), (4, 5), (3, 1)] 
--			= [(0, [1, 2]), (3, [2, 1]), (4, [5])]
gather :: Ord a => [(a, b)] -> [(a, [b])]
gather	xx	
 	= Map.toList 
	$ foldr (\(k, v) m -> 
			Map.insertWith 
				(\x xs -> x ++ xs) 
				k [v] m) 
		Map.empty 
		xx


-- Stats ------------------------------------------------------------------------------------------
-- | Compute statistics for many-valued aspects.
makeAspectStats :: Aspect [] units -> Aspect Stats units
makeAspectStats aspect
 = case aspect of
	Time timed dat	-> Time timed (makeStats dat)
	Size sized dat	-> Size sized (makeStats dat)
	Used used  dat	-> Used used  (makeStats dat)


-- Comparison -------------------------------------------------------------------------------------
makeAspectComparisons 
	:: Real units 
	=> [Aspect Stats units] -> [Aspect Stats units] -> [Aspect StatsComparison units]
	
makeAspectComparisons base new
	= map (makeAspectComparison base) new


-- | Lookup the baseline result for some aspect and produce a comparison.
makeAspectComparison
	:: Real units
	=> [Aspect Stats units] -> Aspect Stats units -> Aspect StatsComparison units

makeAspectComparison base aspect
 = case lookupAspect base aspect of
	Just aspectBase	-> liftAspect2 makeStatsComparison    aspectBase aspect
	Nothing		-> liftAspect  makeStatsComparisonNew aspect


lookupAspect :: [Aspect Stats units] -> Aspect Stats units -> Maybe (Aspect Stats units)
lookupAspect base aspect
 = let	detail	= fst $ splitAspect aspect
   in	find (\a -> (fst $ splitAspect a) == detail) base

