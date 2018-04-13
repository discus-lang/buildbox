
-- | The detail is the name of an `Aspect` seprate from its data.
module BuildBox.Data.Detail
        ( Detail (..)
        , Timed  (..)
        , Used   (..)
        , Sized  (..))
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
        TotalWall       -> string "runtime        (wall clock)"
        TotalCpu        -> string "runtime        (cpu usage)"
        TotalSys        -> string "runtime        (sys usage)"

        KernelWall      -> string "kernel runtime (wall clock)"
        KernelCpu       -> string "kernel runtime (cpu usage)"
        KernelSys       -> string "kernel runtime (sys usage)"


-- | Some resource used during execution.
data Used
        = HeapMax
        | HeapAlloc
        deriving (Eq, Ord, Show, Read, Enum)

instance Pretty Used where
 ppr used
  = case used of
        HeapMax         -> string "maximum heap usage"
        HeapAlloc       -> string "heap allocation"


-- | Some static size of the benchmark that isn't affected during the run.
data Sized
        = ExeSize
        deriving (Eq, Ord, Show, Read, Enum)

instance Pretty Sized where
 ppr sized
  = case sized of
        ExeSize         -> string "executable size"


