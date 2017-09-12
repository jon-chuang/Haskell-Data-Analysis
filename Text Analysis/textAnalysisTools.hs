{-# LANGUAGE LambdaCase #-}
module TextAnalysisTools where

import Data.Char
import Data.Ord
import Data.List as L
import Data.HashMap.Lazy as HM

import Control.Parallel.Strategies
import System.IO
import Data.Time

import DescStats

---------------------------- Classification Produres -----------------------------

guessAuthorsUsing nTotal trainingData mapList n = do
  let table = [cosineSim a b |  a <- (mapList !! (n - 1)), b <- (mapList !! (n - 1))] 
  return $ L.map (guessAuthor nTotal trainingData table)
                 [0..((length $ head mapList) - 1) ]

guessAuthorWithRange nTotal trainingData mapList range = do
  let table' = [[cosineSim a b | a <- (mapList !! (k - 1)), b <- (mapList !! (k - 1))]
               | k <- [1..10]]
  return $ L.map (guessAuthor' nTotal range trainingData table')
                 [0..((length $ head mapList) - 1)]

guessAuthor :: Int -> [(Int, [Int])] -> [Double] -> Int -> Int
guessAuthor nTotal trainingData table toGuess =
  fst $ maximumBy
        (comparing (mean . L.map (\x -> table !! (nTotal * toGuess + x)) . snd ))
        trainingData

guessAuthor' :: Int -> [Int] -> [(Int, [Int])] -> [[Double]] -> Int -> Int
guessAuthor' nTotal range trainingData table toGuess =
  fst $ maximumBy (comparing $ mean . L.map meanOverNgrams . snd ) trainingData
  where meanOverNgrams = mean $ L.map
          (\ngramNo -> ((table !! (ngramNo - 1)) !!  (nTotal * toGuess + x)) range)

---------------------------- Stateful Pre-processing -----------------------------

tableCategories filePaths = do
   docs <- mapM getTexts filePaths
   return $ L.map (read . head) docs :: IO [Int]

mapNgrams filePaths = do
  docs <- mapM getTexts filePaths
  let texts = L.map processDocument docs 
      ngrams = L.map (\n -> (L.map (L.map unwords . ngram n) texts)) [1..10]
      ngramList = L.map (L.map mapFreqs) ngrams    
  return ngramList

mapAugmentedNgrams filePaths = do
  docs <- mapM getTexts filePaths 
  let texts      = L.map processDocument docs 
      ngrams     = L.map (\n -> (L.map (L.map unwords . ngram n) texts)) [1..10]
      ngramList  = L.map (L.map mapFreqs) ngrams
      scores     = L.map (\n -> zipWith (scorer (ngramList !! n)) (ngrams !! n)
                           [0..(length filePaths - 1)]) [0..9]
      augmentedNgramList = zipWith (zipWith tfidfier) scores ngramList
  return augmentedNgramList

getTexts :: FilePath -> IO [String]
getTexts filePath = do
  filecontents <- readFile filePath
  return (lines filecontents)

------------------------------- Text Preprocessing -----------------------------------

ngram :: Int -> [String] -> [[String]]
ngram n xs 
  | drop (n - 1) xs /= [] = take n xs : ngram n (drop 1 xs)
  | otherwise             = []

mapFreqs :: [String] -> HashMap String Double
mapFreqs ngrams = L.foldl (\myMap ngram' -> insertWith (+) ngram' 1.0 myMap) empty ngrams

processDocument :: [String] -> [String]
processDocument text =
  words $ L.filter (\ch -> isAsciiLower ch || ch == ' ') $
      L.map toLower $ unwords $ tail $ tail text
      
----------------------------------- Text Analysis  -----------------------------------

sumSq map1 map2 = sum $ elems $ intersectionWith (*) map1 map2

cosineSim a b = ab / (sqrt aa * sqrt bb)
  where ab = sumSq a b; aa = sumSq a a; bb = sumSq b b

tf :: String -> HashMap String Double -> Double
tf word myMap = let maxfreq = snd $ maximumBy (comparing snd) $ toList myMap in
  case (HM.lookup word myMap) of
    Nothing  -> 0.5
    Just a   -> (a / maxfreq) * 0.5 + 0.5

idf :: String -> [HashMap String Double] -> Double
idf word list = log $ (/) (realToFrac $ length list) $ 
  1 + (sum $ L.map ((\case Nothing -> 0; _ -> 1) . (HM.lookup word)) list)

tfidf :: String -> [HashMap String Double] -> [Double]
tfidf word list = L.map ((* idf word list) . tf word) list

scorer :: [HashMap String Double] -> [String] -> Int -> [(String, Double)]
scorer myMapList words n = L.map (\x -> (x, (L.map flatten $ tfidf x myMapList) !! n)) words
        where flatten = (\x -> if x < 0 then 0 else x)

tfidfier :: [(String, Double)] -> HashMap String Double -> HashMap String Double
tfidfier tfidfscores ngramMap = 
  foldl (\myMap x -> HM.insert (fst x) (snd x) myMap) ngramMap tfidfscores

----------------------------- General Tools ----------------------------------------

runTimeTest action = do
  start <- getCurrentTime
  action
  end <- getCurrentTime
  putStrLn $ "Completed. Time taken: " ++ (show $ diffUTCTime end start)
  

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
  b <- rpar (f a)
  bs <- parMap' f as
  return (b:bs)
