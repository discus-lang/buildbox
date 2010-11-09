{-# LANGUAGE StandaloneDeriving, GADTs, MultiParamTypeClasses, FunctionalDependencies, 
	     FlexibleInstances,  RankNTypes, UndecidableInstances #-}

-- | Physical units of measure.
module BuildBox.Aspect.Units
	( 
	  -- * The unit types
	  Seconds	(..)
	, Bytes		(..)

	  -- * IsUnits
	, IsUnits 	(..)

	  -- * HasUnits
	, HasUnits 	(..)
	
	  -- * WithUnits wrappers
	, WithUnits	(..)
	, secs
	, bytes
	, appWithUnits
	, liftWithUnits
	, liftsWithUnits
	, liftsWithUnits2

	  -- * Unit-preserving collation
	, Collatable	(..)
	, collateWithUnits)
	
where
import BuildBox.Aspect.Single
import BuildBox.Data.Dividable
import BuildBox.Pretty
import Data.Maybe


-- Unit types -------------------------------------------------------------------------------------
-- | Seconds of time.
data Seconds	= Seconds Double
		deriving (Read, Show, Ord, Eq)

instance Real Seconds where
	toRational (Seconds s1) 	= toRational s1

instance Dividable Seconds where
	divide (Seconds s1) (Seconds s2) = Seconds (s1 / s2)	

instance Num Seconds where
	(+) (Seconds f1) (Seconds f2)	= Seconds (f1 + f2)
	(-) (Seconds f1) (Seconds f2)	= Seconds (f1 - f2)
	(*) (Seconds f1) (Seconds f2)	= Seconds (f1 * f2)
	abs (Seconds f1) 		= Seconds (abs f1)
	signum (Seconds f1)		= Seconds (signum f1)
	fromInteger i			= Seconds (fromInteger i)
	
instance Pretty Seconds where
	ppr (Seconds f)			
		= fromMaybe (text (show f))
		$ pprEngDouble "s" f


-- | Bytes of data.
data Bytes	= Bytes	  Integer
		deriving (Read, Show, Ord, Eq)

instance Real Bytes where
	toRational (Bytes b1)		= toRational b1

instance Dividable Bytes where
	divide (Bytes s1) (Bytes s2)	= Bytes (s1 `div` s2)

instance Num Bytes where
	(+) (Bytes f1) (Bytes f2)	= Bytes (f1 + f2)
	(-) (Bytes f1) (Bytes f2)	= Bytes (f1 - f2)
	(*) (Bytes f1) (Bytes f2)	= Bytes (f1 * f2)
	abs (Bytes f1) 			= Bytes (abs f1)
	signum (Bytes f1)		= Bytes (signum f1)
	fromInteger i			= Bytes (fromInteger i)

instance Pretty Bytes where
	ppr (Bytes b)			
		= fromMaybe (text (show b))
		$ pprEngInteger "B" b
	

-- Type classes -----------------------------------------------------------------------------------
-- | Represents the units used for some thing.
data IsUnits a where
	IsSeconds 	:: IsUnits Seconds
	IsBytes		:: IsUnits Bytes	

class HasUnits a a => Units a where
	isUnits :: a -> Maybe (IsUnits a)

instance Units Seconds where
	isUnits s		= hasUnits s

instance Units Bytes where
	isUnits s		= hasUnits s


-- | Determine the units used by the elements of some collection, 
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
deriving instance (Read (t Bytes), Read (t Seconds)) => Read (WithUnits t)

instance  (Pretty (t Bytes), Pretty (t Seconds))
	=> Pretty (WithUnits t) where
 ppr withUnits
  = case withUnits of
	WithSeconds s	-> ppr s
	WithBytes   b	-> ppr b

-- | Helpful wrapper for constructing seconds-valued aspect data. Examples:
-- 
--   @Time TotalWall \`secs\` 10  ::  WithUnits (Aspect Single)@
-- 
secs 	:: (Single Seconds -> c Single Seconds) 
	-> Double -> WithUnits (c Single)
secs mk f  = WithSeconds (mk (Single (Seconds f)))


-- | Similar to `secs`.
bytes 	:: (Single Bytes -> c Single Bytes) 
	-> Integer -> WithUnits (c Single)
bytes mk b = WithBytes   (mk (Single (Bytes b)))


-- | Apply a function to unit-wrapped data
appWithUnits
	:: (forall units. Real units => t1 units -> b)
	-> WithUnits t1 -> b
	
appWithUnits f withUnits
 = case withUnits of
	WithSeconds dat	-> f dat
	WithBytes   dat	-> f dat


-- | Apply a function to unit-wrapped data.
liftWithUnits 
	:: (forall units. Real units => t1 units -> t2 units)
	-> WithUnits t1 -> WithUnits t2

liftWithUnits f withUnits
 = case withUnits of
	WithSeconds dat	-> WithSeconds (f dat)
	WithBytes   dat -> WithBytes   (f dat)


-- | Transform values of each unit type as a group.
liftsWithUnits 
	:: (forall units. Real units => [t1 units] -> [t2 units]) 
	-> [WithUnits t1] -> [WithUnits t2]

liftsWithUnits f us
  = let	asSeconds	= [a | WithSeconds a	<- us]
	asBytes		= [a | WithBytes   a	<- us]

    in	   (map WithSeconds $ f asSeconds)
	++ (map WithBytes   $ f asBytes)
	

-- | Transform values of each unit type as a group
liftsWithUnits2
	:: (forall units. Real units => [t1 units] -> [t2 units] -> [t3 units])
	-> [WithUnits t1] -> [WithUnits t2] -> [WithUnits t3]
	
liftsWithUnits2 f as bs
 = let	asSeconds	= [a | WithSeconds a	<- as]
	bsSeconds	= [b | WithSeconds b	<- bs]

	asBytes		= [a | WithBytes   a	<- as]
	bsBytes		= [b | WithBytes   b	<- bs]
	
   in	(map WithSeconds $ f asSeconds bsSeconds)
    ++	(map WithBytes   $ f asBytes   bsBytes)



-- Unit-safe collation ----------------------------------------------------------------------------
-- | Collate some data, while preserving units.
class Collatable t where
	collate :: forall a. HasUnits a a 
		=> [t Single a] -> [t [] a]


-- | Collate some data.
--
--  @
-- collateWithUnits  [ Time KernelCpu \`secs\`  5
--                   , Time KernelCpu \`secs\`  10
--                   , Time TotalWall \`secs\`  55
--                   , Size ExeSize   \`bytes\` 100884
--                   , Time TotalWall \`secs\`  52 ]
-- =>
--                   [ WithSeconds (Time KernelCpu [Seconds 5.0,  Seconds 10.0])
--                   , WithSeconds (Time TotalWall [Seconds 55.0, Seconds 52.0])
--                   , WithBytes   (Size ExeSize [Bytes 1024])]
--  @
-- 
collateWithUnits :: Collatable c => [WithUnits (c Single)] -> [WithUnits (c [])]
collateWithUnits as
  = let	asSeconds	= [a | WithSeconds a	<- as]
	asBytes		= [a | WithBytes   a	<- as]

    in	   (map WithSeconds $ collate asSeconds)
	++ (map WithBytes   $ collate asBytes)

