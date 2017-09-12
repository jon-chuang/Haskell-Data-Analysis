{-# LANGUAGE LambdaCase #-}
import Data.List as L
import Data.HashMap.Lazy as HM

import Control.Parallel.Strategies
import System.IO
import System.Directory

import TextAnalysisTools
{-
This is a program which classifies text files based on their word-frequency cosine similarity
or tfidf CS wrt existing groups of classified traning data.

Please place your text files in folders named Data/Training and Data/Testing
in the same directory as this program.
Please place the category of your file on the first line followed by a new line.

Alternatively, you can use the insertCat tool to do the insertion for you
by passing in the following parameters to the main method in
insertCat.hs from the command line:
<Directory name> <# of files for training> <# of files for testing>
Here, <directory name> is the name of the parent folder containing folders of text files.
Each folder in this parent folder should contain the text files of one category.
-}

main = do
  putStr $ "Please enter 'A' for augmented cosine similarity.\n" ++ 
             "For ordinary consine similarity, please press enter: "
  hFlush stdout
  input <- getLine
  if (input == "A" || input == "a") then main' mapAugmentedNgrams "Augmented "
  else if (input == "")             then main' mapNgrams ""
  else                                   main

main' chosenMap str = do
  putStr ("Please input the n-grams you would like to use, seperated by spaces: ")
  hFlush stdout
  input <- fmap words getLine
  let ns = L.map read input :: [Int]

  let ranges = [[]] :: [[Int]]
  -- To be filled up at the user's pleasure. Be forewarned: it is painfully slow.
  
  filePathsTraining <-
    fmap (L.map ("Data/Training/" ++ ) . reverse . L.filter (\x -> (x/=".") && (x/=".."))) $
         getDirectoryContents "Data/Training"
  filePathsTesting  <-
    fmap (L.map ("Data/Testing/" ++ ) . reverse . L.filter (\x -> (x/=".") && (x/=".."))) $
         getDirectoryContents "Data/Testing"
  
  let filePaths = filePathsTraining ++ filePathsTesting
  
  tA <- tableCategories filePaths
  
  let nTraining = length filePathsTraining
      nTotal    = length filePaths
      trainingDataRaw = take nTraining tA
      (acc, trainingData') =
          foldl (\(value, map) key -> (value + 1, insertWith (++) key [value] map))
                (0, empty) trainingDataRaw 
      trainingData = toList trainingData' :: [(Int, [Int])]
      
  mapList <- chosenMap filePaths
  gAUs <- mapM (guessAuthorsUsing    nTotal trainingData mapList) ns
  gAs  <- mapM (guessAuthorWithRange nTotal trainingData mapList) ranges

  let printN (n, classifierResults) =
        putStrLn $
          str ++ (show n) ++ "-gram classification: " ++
          show score ++ "/" ++ (show $ length filePathsTesting) ++ " successful"
          where score = (foldl (\acc bool -> if bool then acc + 1 else acc) 0 $
                           drop nTraining $  zipWith (==) tA classifierResults)

  -- To run parallel evaluation, comment out the final 2 lines starting with mapM_ 
  -- remove putStrLn from the printN definition and include the following two lines:
  -- let single' = runEval $ parMap' (\x -> (printN x :: String)) $ zip ns gAUs
  -- mapM_ putStrLn $ single'

  mapM_ runTimeTest $ L.map (\x -> printN x >> hFlush stdout) $ zip ns     gAUs
  mapM_ runTimeTest $ L.map (\x -> printN x >> hFlush stdout) $ zip ranges gAs
  return ()
