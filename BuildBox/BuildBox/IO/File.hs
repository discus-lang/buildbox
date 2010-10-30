
module BuildBox.IO.File
	(atomicWriteFile)
where


atomicWriteFile :: FilePath -> String -> IO ()
atomicWriteFile filePath str
 = do	writeFile filePath str
