{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GADTs, FlexibleContexts #-}

module BuildBox.Aspect.Aspect
	(Aspect	(..))
where
import BuildBox.Aspect.Units
import BuildBox.Aspect.Detail
import qualified Data.Map	as Map
import Data.Map			(Map)

-- | An aspect of a benchmark that we can measure.
--   Aspects are parameterised over a type constructor 't' that can be a collection
--   used to store multiple readings. For a single reading used the `Single` type.
data Aspect t units where
	Time	:: Timed	-> t Seconds	-> Aspect t Seconds
	Size	:: Sized	-> t Bytes	-> Aspect t Bytes
	Used	:: Used		-> t Bytes	-> Aspect t Bytes

deriving instance Show (t units) => Show (Aspect t units)	
-- TODO: read instance


-- | Split an aspect into its named detail and value.
splitAspect :: Aspect t units -> (Detail, t units)
splitAspect aa
 = case aa of
	Time timed val		-> (DetailTimed timed, val)
	Size sized val		-> (DetailSized sized, val)
	Used used  val		-> (DetailUsed  used,  val)


-- | Make an aspect from named detail and data.
--   If the detail doesn't match the units of the data then `Nothing`.
makeAspect
	:: HasUnits (t units) units 
	=> Detail -> t units -> Maybe (Aspect t units)

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


-- Collate ----------------------------------------------------------------------------------------
instance Collatable Aspect where
 collate as
  = let	-- This Just match will always succeed provided the implementation of gather is correct.
	Just as' = sequence 
		 $ map (uncurry makeAspect) 
		 $ gather [(detail, val) | (detail, (Single val)) <- map splitAspect as]
    in	as'


collateWithUnits :: Collatable c => [WithUnits (c Single)] -> [WithUnits (c [])]
collateWithUnits as
  = let	asSeconds	= [a | WithSeconds a	<- as]
	asBytes		= [a | WithBytes   a	<- as]

    in	   (map WithSeconds $ collate asSeconds)
	++ (map WithBytes   $ collate asBytes)


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
