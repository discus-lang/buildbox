
module BuildBox.IO.File
	(atomicWriteFile)
where

-- | Should atomically write a file by writing it to a tmp file then renaming it.
--   TODO: Does not yet work as advertised.
atomicWriteFile :: FilePath -> String -> IO ()
atomicWriteFile filePath str
 = do	writeFile filePath str
