
-- | An aspect is a piece of data obtained from running a benchmark, like its 
--   total runtime, heap usage, or executable size. Aspects have physical
--   units, so runtime is in seconds, and executable size is in bytes. The type system
--   ensures that you can't mess up the units, such as by treating executable size as though it was
--   measured in seconds.
--
--   Aspects are also parameterised over a carrier constructor, which is the collection type
--   used to store the data. For single valued data use the `Single` constructor. For multi valued
--   data use the @[]@ (the list constructor). Use this when you have several readings for
--   the same benchmark, like runtimes from multiple independent runs. 
--   Other useful constructors are `Stats`, `Comparison` and `StatsComparison`.
--
--   Once you have a many-valued aspect, you can use `makeAspectStats` to compute statistics
--   from the data.
-- 
--   Here is a worked example:
--
--  @ 
-- -- This is our original, single valued data.
--someData :: [`WithUnits` (`Aspect` `Single`)]
--someData =  [ `Time` `TotalWall` \``secs`\`  100
--            , `Time` `TotalWall` \``secs`\`  85
--            , `Size` `ExeSize`   \``bytes`\` 1024
--            , `Used` `HeapMax`   \``bytes`\` 100000
--            , `Used` `HeapMax`   \``bytes`\` 100100]
--  @
--  
--  @
-- -- Collate the data, which groups all the readings for the same aspect into a list.
-- -- Note that the carrier constructor is now [].
--collated  :: [`WithUnits` (`Aspect` [])]
--collated  = `collateWithUnits` someData
-- ...
--show collated 
--  =>   [ `WithSeconds` (`Time` `TotalWall` [`Seconds` 100.0, `Seconds` 85.0])
--       , `WithBytes`   (`Used` `HeapMax`   [`Bytes` 100000,  `Bytes` 100100])
--       , `WithBytes`   (`Size` `ExeSize`   [`Bytes` 1024])]
--  @
--
--  @
-- -- Extract statistics from the collated data.
--analysed   :: [`WithUnits` (`Aspect` `Stats`)]
--analysed   =  map (`liftWithUnits` `makeAspectStats`) collated
-- ...
--show analysed
--  =>   [ `WithSeconds` (`Time` `TotalWall` (`Stats` {`statsMin` = `Seconds` 85.0, `statsAvg` = `Seconds` 92.5, `statsMax` = `Seconds` 100.0}))
--       , `WithBytes`   (`Used` `HeapMax`   (`Stats` {`statsMin` = `Bytes` 100000, `statsAvg` = `Bytes` 100050, `statsMax` = `Bytes` 100100}))
--       , `WithBytes`   (`Size` `ExeSize`   (`Stats` {`statsMin` = `Bytes` 1024,   `statsAvg` = `Bytes` 1024,   `statsMax` = `Bytes` 1024}))]
-- @
module BuildBox.Aspect
	( module BuildBox.Aspect.Units
	, module BuildBox.Aspect.Detail
	, module BuildBox.Aspect.Stats
	, module BuildBox.Aspect.Single
	, module BuildBox.Aspect.Comparison

	-- * Aspects
	, Aspect	(..)
	, makeAspect
	, splitAspect

	-- * Statistics and comparisons
	, makeAspectStats
	, makeAspectComparison
	, makeAspectComparisons

	-- * Application functions
	, appAspect
	, appAspectWithUnits

	-- * Lifting functions
	, liftAspect
	, liftAspect2)
where
import BuildBox.Aspect.Aspect
import BuildBox.Aspect.Detail
import BuildBox.Aspect.Stats
import BuildBox.Aspect.Units
import BuildBox.Aspect.Single
import BuildBox.Aspect.Comparison





