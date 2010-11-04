
module BuildBox.Benchmark.Aspect.Units
	( Units	(..))
where
import BuildBox.Pretty

data Units
	= Seconds
	| Bytes
	deriving (Eq, Show)

instance Pretty Units where
 ppr units
  = case units of
	Seconds{} 	-> ppr "s"
	Bytes{}		-> ppr "B"
