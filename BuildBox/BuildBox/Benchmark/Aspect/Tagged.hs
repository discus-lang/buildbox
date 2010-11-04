{-# LANGUAGE RankNTypes #-}
module BuildBox.Benchmark.Aspect.Tagged
	( Tagged(..)
	, liftTagged
	, mapTagged
	, mapGrouped )
where
	
data Tagged c
	= TagFloat	(c Float)
	| TagInteger	(c Integer)


liftTagged :: (forall a. c a -> c a) -> Tagged c -> Tagged c
liftTagged f tt
 = case tt of
	TagFloat   x	-> TagFloat   (f x)
	TagInteger x	-> TagInteger (f x)


mapTagged :: (forall a. a -> a) -> [Tagged c] -> [Tagged c]
mapTagged f []	= []
mapTagged f (x : xs)
	= liftTagged f x : mapTagged f xs


mapGrouped
	:: (forall a. [c a] -> [c a])
	-> [Tagged c] -> [Tagged c]
	
mapGrouped  f xs
 = let	asFloat		= [x | TagFloat x 	<- xs]
	asInteger	= [x | TagInteger x 	<- xs]
	
   in	(map TagFloat   $ f asFloat)
    ++	(map TagInteger $ f asInteger)
