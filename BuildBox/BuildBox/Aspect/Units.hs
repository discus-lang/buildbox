
{-# LANGUAGE StandaloneDeriving, GADTs, MultiParamTypeClasses, FunctionalDependencies, 
	     FlexibleInstances,  RankNTypes, UndecidableInstances #-}

-- | Working with collections of values, where the values are associated with physical units of measure.
module BuildBox.Aspect.Units
	( 
	  -- * The unit types
	  Seconds	(..)
	, Bytes		(..)

	  -- * Singleton collections
	, Single	(..)

	  -- * Type classes
	, IsUnits 	(..)
	, HasUnits 	(..)
	
	  -- * WithUnits wrappers
	, WithUnits	(..)
	, secs
	, bytes
	, liftsWithUnits
	, liftWithUnits

	  -- * Unit-preserving collation
	, Collatable	(..)
	, collateWithUnits)
	
where
import BuildBox.Data.Dividable


-- Unit types -------------------------------------------------------------------------------------
-- | Seconds of time.
data Seconds	= Seconds Float	
		deriving (Read, Show, Ord, Eq)

instance Dividable Seconds where
	divide (Seconds s1) (Seconds s2)
		= Seconds (s1 / s2)
	
instance Num Seconds where
	(+) (Seconds f1) (Seconds f2)	= Seconds (f1 + f2)
	(-) (Seconds f1) (Seconds f2)	= Seconds (f1 - f2)
	(*) (Seconds f1) (Seconds f2)	= Seconds (f1 * f2)
	abs (Seconds f1) 		= Seconds (abs f1)
	signum (Seconds f1)		= Seconds (signum f1)
	fromInteger int			= Seconds (fromInteger int)
	

-- | Bytes of data.
data Bytes	= Bytes	  Integer
		deriving (Read, Show, Ord, Eq)

instance Dividable Bytes where
	divide (Bytes b1) (Bytes b2)
		= Bytes (b1 `div` b2)

instance Num Bytes where
	(+) (Bytes f1) (Bytes f2)	= Bytes (f1 + f2)
	(-) (Bytes f1) (Bytes f2)	= Bytes (f1 - f2)
	(*) (Bytes f1) (Bytes f2)	= Bytes (f1 * f2)
	abs (Bytes f1) 			= Bytes (abs f1)
	signum (Bytes f1)		= Bytes (signum f1)
	fromInteger int			= Bytes (fromInteger int)


-- Singleton collections --------------------------------------------------------------------------
-- | A single valued piece of data.
data Single a 
	= Single a
	deriving (Read, Show)

instance Num a => Num (Single a) where
	(+) (Single f1) (Single f2)	= Single (f1 + f2)
	(-) (Single f1) (Single f2)	= Single (f1 - f2)
	(*) (Single f1) (Single f2)	= Single (f1 * f2)
	abs (Single f1) 		= Single (abs f1)
	signum (Single f1)		= Single (signum f1)
	fromInteger int			= Single (fromInteger int)

instance Eq a => Eq (Single a) where
	(==) (Single f1) (Single f2)	= f1 == f2


-- Type classes -----------------------------------------------------------------------------------
-- | Refies the units used for some thing.
data IsUnits a where
	IsSeconds 	:: IsUnits Seconds
	IsBytes		:: IsUnits Bytes	


-- | Determine the units of the elements of some collection, 
--   by inspecting the elements directly.
--   Returns `Nothing` when applied to empty collections, as they have no units.
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


-- WithUnits --------------------------------------------------------------------------------------
-- | A wrapper type used to store data of varying physical units in a homogenous collection structure.
data WithUnits t where
	WithSeconds	:: t Seconds	-> WithUnits t
	WithBytes	:: t Bytes	-> WithUnits t
	
deriving instance (Show (t Bytes), Show (t Seconds)) => Show (WithUnits t)


-- | Helpful wrapper for constructing seconds-valued aspect data. Examples:
-- 
--   @Time TotalWall \`secs\` 10  ::  WithUnits (Aspect Single)@
-- 
secs 	:: (Single Seconds -> c Single Seconds) 
	-> Float -> WithUnits (c Single)
secs mk f  = WithSeconds (mk (Single (Seconds f)))


bytes 	:: (Single Bytes -> c Single Bytes) 
	-> Integer -> WithUnits (c Single)
bytes mk b = WithBytes   (mk (Single (Bytes b)))


liftWithUnits 
	:: (forall units. HasUnits units units => t1 units -> t2 units) 
	-> WithUnits t1 -> WithUnits t2

liftWithUnits f withUnits
 = case withUnits of
	WithSeconds dat	-> WithSeconds (f dat)
	WithBytes   dat -> WithBytes   (f dat)


liftsWithUnits 
	:: (forall units. HasUnits units units => [t1 units] -> [t2 units]) 
	-> [WithUnits t1] -> [WithUnits t2]

liftsWithUnits f us
  = let	asSeconds	= [a | WithSeconds a	<- us]
	asBytes		= [a | WithBytes   a	<- us]

    in	   (map WithSeconds $ f asSeconds)
	++ (map WithBytes   $ f asBytes)


-- Unit-safe collation ----------------------------------------------------------------------------
-- | Collate some data, while preserving units.
class Collatable t where
	collate :: forall a. HasUnits a a 
		=> [t Single a] -> [t [] a]


-- | Collate several values for the same aspect.
--
--  @
--     collateWithUnits  [ Time KernelCpu \`secs\` 5
--                       , Time KernelCpu \`secs\` 10
--                       , Time TotalWall \`secs\` 55
--                       , Size ExeSize   \`bytes\` 100884
--                       , Time TotalWall \`secs\` 52 ]
--     =>
--                       [ WithSeconds (Time KernelCpu [Seconds 5.0,  Seconds 10.0])
--                       , WithSeconds (Time TotalWall [Seconds 55.0, Seconds 52.0])
--                       , WithBytes   (Size ExeSize [Bytes 1024])]
--  @
-- 
collateWithUnits :: Collatable c => [WithUnits (c Single)] -> [WithUnits (c [])]
collateWithUnits as
  = let	asSeconds	= [a | WithSeconds a	<- as]
	asBytes		= [a | WithBytes   a	<- as]

    in	   (map WithSeconds $ collate asSeconds)
	++ (map WithBytes   $ collate asBytes)

