
module BuildBox.Aspect.Detail
	( Detail (..)
	, Timed	 (..)
	, Used	 (..)
	, Sized	 (..))
where
instance BuildBox.Pretty

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
	TotalWall	-> "runtime (wall clock)"
	TotalCpu	-> "runtime (cpu usage)"
	TotalSys	-> "runtime (sys usage)"

	KernelWall	-> "kernel runtime (wall clock)"
	KernelCpu	-> "kernel runtime (cpu usage)"
	KernelSys	-> "kernel runtime (sys usage)"
		

-- | Some resource used during execution.
data Used
	= HeapMax
	| HeapAlloc
	deriving (Eq, Ord, Show, Read, Enum)
	
instance Pretty Used where
 ppr used
  = case used of
	HeapMax		-> "maximum heap usage"
	HeapAlloc	-> "heap allocation"
	
	
-- | Some static size of the benchmark that isn't affected during the run.
data Sized
	= ExeSize
	deriving (Eq, Ord, Show, Read, Enum)
	
instance Pretty Sized where
 ppr sized
  = case sized of
	ExeSize		-> "executable size"
	
	