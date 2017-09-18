module InsertCat where

import Data.List as L
import System.Directory
import Control.DeepSeq
import Debug.Trace

help = do
  putStrLn $ "Please call the main method with the following arguments:\n" ++
    "<Directory name> <# of files for training> <# of files for testing>"

main :: [Char] -> Int -> Int -> IO ()
main dir nTrain nTest = do
  mapM_ (createDirectoryIfMissing True)
        ["Data/Training", "Data/Testing"]
  dirNames <- fmap (reverse . init . init) $
                       getDirectoryContents dir
  foldl1 (>>) $ zipWith (appendToTopTrain nTrain) [1..(length dirNames)]
                (map ((dir ++ "/") ++) dirNames)
  foldl1 (>>) $ zipWith (appendToTopTest nTrain nTest) [1..(length dirNames)]
                (map ((dir ++ "/") ++) dirNames)
  

appendToTopTrain nTrain n dir = do
  fileNames <- fmap (reverse . init . init) $
                       getDirectoryContents dir
  mapM_ (transformFile "Training/" dir n) (take nTrain fileNames)


appendToTopTest nTrain nTest n dir = do
  fileNames <- fmap (reverse . (filter (\x -> (x/=".") && (x/=".."))) $
                       getDirectoryContents dir
  mapM_ (transformFile "Testing/" dir n) (take nTest $ drop nTrain fileNames)

transformFile trainOrTest dir n fileName = do
  contents <- readFile ((dir ++ "/") ++ fileName)
  let newFilePath = "Data/" ++ trainOrTest ++ fileName
  print ((head $ lines contents) ++ " @ " ++ fileName)
  let newContents = show n ++ "\n" ++ contents
  writeFile (newFilePath) newContents    
