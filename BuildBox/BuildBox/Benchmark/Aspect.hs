{-# LANGUAGE PatternGuards, FlexibleInstances, FlexibleContexts, GADTs, MultiParamTypeClasses #-}

-- | Dealing with aspects of timing results.
module BuildBox.Benchmark.Aspect
where
import BuildBox.Benchmark.Aspect.Mode
import BuildBox.Benchmark.Aspect.Sort
import BuildBox.Benchmark.Aspect.Units
import BuildBox.Pretty
import Control.Monad
import Data.Map			(Map)
import qualified Data.Map	as Map


-- AspectDatum ------------------------------------------------------------------------------------
-- | Used to reify the type of data stored in an aspect.
data SomeData t a where
	SomeFloat   	:: t Float   -> SomeData t Float
	SomeInteger	:: t Integer -> SomeData t Integer

class AspectData t a where
	takeData 	:: t a -> SomeData t a
	
instance AspectData t Float where
	takeData f	= SomeFloat f

instance AspectData t Integer where
	takeData i	= SomeInteger i
	

-- Tagged -----------------------------------------------------------------------------------------
data Tagged c
	= TagFloat	(c Float)
	| TagInteger	(c Integer)

instance Show (Tagged (Aspect Single)) where
	show ta
	 = case ta of
		TagFloat aspect		-> show (splitAspect aspect)
		TagInteger aspect	-> show (splitAspect aspect)


-- Aspect -----------------------------------------------------------------------------------------
-- | Typed benchmark aspects.
data Aspect t a where
	AspectRuntimeWall 	:: t Float	-> Aspect t Float
	AspectRuntimeKernelWall	:: t Float	-> Aspect t Float
	AspectRuntimeKernelCpu	:: t Float	-> Aspect t Float
	AspectRuntimeKernelSys	:: t Float	-> Aspect t Float
	AspectBinarySize	:: t Integer	-> Aspect t Integer


-- | Make an aspect form its sort and data.
--   If the sort doesn't match the data then `Nothing`.
makeAspect :: AspectData t a => AspectSort -> t a -> Maybe (Aspect t a)
makeAspect sort val
 = case takeData val of
	SomeFloat _
	 -> case sort of
		RuntimeWall		-> Just (AspectRuntimeWall val)
		RuntimeKernelWall	-> Just (AspectRuntimeKernelWall val)
		RuntimeKernelCpu	-> Just (AspectRuntimeKernelCpu val)
		RuntimeKernelSys	-> Just (AspectRuntimeKernelSys val)
		_			-> Nothing

	SomeInteger _
	 -> case sort of
		BinarySize		-> Just (AspectBinarySize val)
		_			-> Nothing


-- | Split an aspect into its sort and data.
splitAspect :: Aspect t a -> (AspectSort, t a)
splitAspect aspect
 = case aspect of
	AspectRuntimeWall v		-> (RuntimeWall, v)
	AspectRuntimeKernelWall v	-> (RuntimeKernelWall, v)
	AspectRuntimeKernelCpu v	-> (RuntimeKernelCpu, v)
	AspectRuntimeKernelSys v	-> (RuntimeKernelSys, v)
	AspectBinarySize v		-> (BinarySize, v)


-- | Take the value of an aspect.
valueOfAspect :: Aspect t a -> t a
valueOfAspect aspect	= snd $ splitAspect aspect

-- | Take the sort of an aspect.
sortOfAspect  :: Aspect t a -> AspectSort
sortOfAspect aspect	= fst $ splitAspect aspect


-- | Collate a list of single aspects into their stats.s
collateAspects 
	:: (Num a, Ord a, Dividable a, AspectData Stats a) 
	=> [Aspect Single a] 
	-> [Aspect Stats a]

collateAspects aspects
 = let	Just stats
		= sequence	
		$ [ makeAspect sort (makeStats $ map valueOfSingle xs) 
			| (sort, xs) <- gather $ map splitAspect aspects ]
   in	stats


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
