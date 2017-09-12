module AuthorsToCats where

import Data.List as L
import System.Directory
import Control.DeepSeq
import Debug.Trace

authorsToCategories = do
    filePaths <- fmap (L.map("Data/" ++) . reverse . init . init) $
                       getDirectoryContents "Data"
    mapM_ transformFile filePaths

transformFile filePath = do
  contents <- readFile filePath
  let newFilePath = take 5 filePath ++ "transformed/" ++ drop 5 filePath
  trace ((head $ lines contents) ++ " @ " ++ filePath) putStr ""
  let newContents = categoriseFile contents ++ contents
  newContents `seq` writeFile (newFilePath) newContents    

categoriseFile file =
  case head (lines file) of 
      "Author: Alexander Hamilton" -> "1\n"
      "Author: John Jay"           -> "2\n"
      "Author: James Madison"      -> "3\n"  
      "Author: Alexander Hamilton and James Madison" -> "4\n"
      "Author: Unknown"            -> "5\n"
      _                            -> "No such author\n"
