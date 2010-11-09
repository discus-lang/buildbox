
-- | The details of benchmark programs that we're interested in.
module BuildBox.Aspect.Detail
	( Detail (..)
	, Timed	 (..)
	, Used	 (..)
	, Sized	 (..))
where
import BuildBox.Pretty

data Detail
	= DetailTimed Timed
	| DetailUsed   Used
	| DetailSized  Sized
	deriving (Eq, Ord, Show, Read)
	

-- | Something that takes time to evaluate.
data Timed
	= TotalWall
	| TotalCpu
	| TotalSys
	| KernelWall
	| KernelCpu 
	| KernelSys
	deriving (Eq, Ord, Show, Read, Enum)
	
instance Pretty Timed where
 ppr timed
  = case timed of
	TotalWall	-> text "runtime        (wall clock)"
	TotalCpu	-> text "runtime        (cpu usage)"
	TotalSys	-> text "runtime        (sys usage)"

	KernelWall	-> text "kernel runtime (wall clock)"
	KernelCpu	-> text "kernel runtime (cpu usage)"
	KernelSys	-> text "kernel runtime (sys usage)"
		

-- | Some resource used during execution.
data Used
	= HeapMax
	| HeapAlloc
	deriving (Eq, Ord, Show, Read, Enum)
	
instance Pretty Used where
 ppr used
  = case used of
	HeapMax		-> text "maximum heap usage"
	HeapAlloc	-> text "heap allocation"
	
	
-- | Some static size of the benchmark that isn't affected during the run.
data Sized
	= ExeSize
	deriving (Eq, Ord, Show, Read, Enum)
	
instance Pretty Sized where
 ppr sized
  = case sized of
	ExeSize		-> text "executable size"
	
	