
module BuildBox.Aspect.Single
	( Single (..))
where 
import BuildBox.Pretty

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
	fromInteger i			= Single (fromInteger i)

instance Eq a => Eq (Single a) where
	(==) (Single f1) (Single f2)	= f1 == f2

instance Pretty a => Pretty (Single a) where
	ppr (Single x)	= ppr x


