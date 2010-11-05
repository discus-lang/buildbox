{-# LANGUAGE 
	StandaloneDeriving, 
	GADTs, 
	MultiParamTypeClasses, 
	FunctionalDependencies, 
	FlexibleInstances, 
	RankNTypes,
	UndecidableInstances #-}

module BuildBox.Aspect.Units
	( Single	(..)
	, Seconds	(..), secs
	, Bytes		(..), bytes
	
	, IsUnits 	(..)
	, WithUnits	(..)
	, HasUnits 	(..)
	
	, Collatable	(..))
where

-- | A single valued piece of data.
data Single a 
	= Single a
	deriving (Read, Show)


-- The sort of units we support.
data Seconds	= Seconds Float		deriving (Read, Show)
data Bytes	= Bytes	  Integer	deriving (Read, Show)


-- | Helpful wrapper to make some seconds thing
--   Use like:
-- 
--   @Time TotalWall `secs` 10  ::  WithUnits (Aspect Single)@
-- 
secs 	:: (Single Seconds -> c Single Seconds) 
	-> Float -> WithUnits (c Single)
secs mk f  = WithSeconds (mk (Single (Seconds f)))

bytes 	:: (Single Bytes -> c Single Bytes) 
	-> Integer -> WithUnits (c Single)
bytes mk b = WithBytes   (mk (Single (Bytes b)))


-- | Refies the sort of units used for some thing.
data IsUnits a where
	IsSeconds 	:: IsUnits Seconds
	IsBytes		:: IsUnits Bytes	


-- | Holds some unit valued thing in a common type.
data WithUnits t where
	WithSeconds	:: t Seconds	-> WithUnits t
	WithBytes	:: t Bytes	-> WithUnits t
	
deriving instance (Show (t Bytes), Show (t Seconds)) => Show (WithUnits t)


-- | Determine the units of something based on its representation.
--   Empty collections have no units, so return Nothing.
class HasUnits a b | a -> b where
	hasUnits :: a -> Maybe (IsUnits b)

instance HasUnits Seconds Seconds where
	hasUnits _		= Just IsSeconds

instance HasUnits Bytes Bytes where
	hasUnits _		= Just IsBytes

instance HasUnits a a => HasUnits (Single a) a where
	hasUnits (Single x)	= hasUnits x

instance HasUnits a a => HasUnits [a] a where
	hasUnits []		= Nothing
	hasUnits (x : _)	= hasUnits x


-- | Collate some data, while preserving units.
class Collatable c where
	collate :: forall a. HasUnits a a 
		=> [c Single a] -> [c [] a]

