
module BuildBox.Data.Dividable
	(Dividable(..))
where

class Dividable a where
	divide :: a -> a -> a
	
instance Dividable Integer where
	divide	= div

instance Dividable Float where
	divide	= (/)
