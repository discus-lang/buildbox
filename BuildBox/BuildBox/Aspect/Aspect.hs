{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GADTs, FlexibleContexts, RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
module BuildBox.Aspect.Aspect
	( Aspect	(..)
	, makeAspect
	, splitAspect
	, transformAspect
	, collateWithUnits
	, makeAspectStats)
where
import BuildBox.Aspect.Units
import BuildBox.Aspect.Detail
import BuildBox.Aspect.Stats
import qualified Data.Map	as Map


data Aspect carrier units where
	Time	:: Timed	-> carrier Seconds	-> Aspect carrier Seconds
	Size	:: Sized	-> carrier Bytes	-> Aspect carrier Bytes
	Used	:: Used		-> carrier Bytes	-> Aspect carrier Bytes

deriving instance Show (carrier units) => Show (Aspect carrier units)	
-- TODO: read instance


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
	:: HasUnits (carrier units) units 
	=> Detail -> carrier units -> Maybe (Aspect carrier units)

makeAspect detail (val :: carrier units)
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
transformAspect
	:: (carrier1 units -> carrier2 units) 
	-> Aspect carrier1 units 
	-> Aspect carrier2 units

transformAspect f aspect
 = case aspect of
	Time timed dat	-> Time timed (f dat)
	Size sized dat	-> Size sized (f dat)
	Used used dat	-> Used used  (f dat)


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


