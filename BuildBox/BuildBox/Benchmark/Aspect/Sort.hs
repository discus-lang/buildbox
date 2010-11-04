
module BuildBox.Benchmark.Aspect.Sort
	( AspectSort(..)
	, ppr
	, unitsOfAspectSort)
where
import BuildBox.Benchmark.Aspect.Units
import BuildBox.Pretty


-- | Represents the sort of aspect we have separately from the data.
data AspectSort 
	= RuntimeWall
	| RuntimeKernelWall
	| RuntimeKernelCpu
	| RuntimeKernelSys
	| BinarySize	
	deriving (Eq, Show, Ord)


instance Pretty AspectSort where
 ppr s
  = case s of
	RuntimeWall		-> text "runtime (wall)"
	RuntimeKernelWall	-> text "runtime (kernel wall)"
	RuntimeKernelCpu	-> text "runtime (kernel cpu)"
	RuntimeKernelSys	-> text "runtime (kernel sys)"
	BinarySize		-> text "binary size"


-- | Get the units used to represent an aspect.
unitsOfAspectSort :: AspectSort -> Units
unitsOfAspectSort s
 = case s of
	RuntimeWall		-> Seconds
	RuntimeKernelWall{}	-> Seconds
	RuntimeKernelCpu{}	-> Seconds
	RuntimeKernelSys{}	-> Seconds
	BinarySize{}	-> Bytes
