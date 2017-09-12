module ClusteringTools where

import Data.List as L
import Data.Map.Strict as M
import Data.Ord
import Graphics.EasyPlot

import DescStats

type Point    = (Double, Double)
type Category = Int
type Labelled = [(Category, Point)]
type Grouped  = [(Category, [Point])]

----------------------------------- Tools -----------------------------------------

centroid :: [Point] -> Point
centroid cluster = (mean $ L.map fst cluster, mean $ L.map snd cluster)

distance a b = sqrt $ (fst a - fst b)^2 + (snd a - snd b)^2 

nearestCentroid :: Labelled -> Point -> Category
nearestCentroid centroids point =
  fst $ L.minimumBy (comparing $ distance point . snd) centroids

group' :: Labelled -> Grouped 
group' labelled = M.toList $ L.foldl
                    (\myMap (k, v) -> insertWith (++) k [v] myMap) empty labelled
                   
-------------------------------- Hierarchical ---------------------------------------

runClosestCentroids :: Int -> Grouped -> Grouped
runClosestCentroids nCat grouped
  | length grouped == nCat = grouped
  | otherwise = 
      runClosestCentroids nCat (merge (closestCentroids grouped) [(-1,[])] grouped)

closestCentroids :: Grouped -> (Category, Category)
closestCentroids grouped = fst $ L.minimumBy (comparing snd) $ concat $
      [[( (aCat, bCat) , distance a b )| (aCat, a) <- (L.filter (/=(bCat, b)) centroids)]
      | (bCat, b) <- centroids]
    where centroids = L.map (\(x, y) -> (x, centroid y)) grouped

merge :: (Category, Category) -> Grouped -> Grouped -> Grouped
merge _ acc [] = acc
merge (cat1, cat2) acc (x:xs)
   | fst x == cat1 || fst x == cat2  = merge (cat1, cat2)
                                       [(fst x, (snd $ head acc) ++ snd x)] xs
   | otherwise                       = x : (merge (cat1, cat2) acc xs)

-----------------------------------K-means -------------------------------------------

improveKmeans :: Labelled -> Labelled
improveKmeans labelled = newLabelled
    where grouped = group' labelled
          centroids = L.map (\(x, y) -> (x, centroid y)) grouped
          newLabelled = L.map (\x -> (nearestCentroid centroids x, x)) $ L.map snd labelled
          
runKmeans labelled newLabelled
   | labelled /= newLabelled  = runKmeans newLabelled (improveKmeans newLabelled)
   | otherwise                = labelled

outputKmeans labelled newLabelled n
  | labelled /= newLabelled  = do 
         plot (JPEG ("Output Kmeans/" ++ show n ++ ".jpg")) $ L.map (Data2D [] []) $
                   L.map snd $ group' labelled
         outputKmeans newLabelled (improveKmeans newLabelled) (n + 1)
  | otherwise                = do
         plot (JPEG ("Output Kmeans/" ++ show n ++ ".jpg")) $ L.map (Data2D [] []) $
                   L.map snd $ group' labelled

  
---------------------------------- Data Generation -------------------------------------
  
reposition :: [Double] -> Double -> Double -> [Double]
reposition values sigma mu = L.map (\x -> sigma * x + mu) scaled
  where
    scaled = L.map (\x -> (x - min)/(max - min) - 0.5) values
    min = minimum values
    max = maximum values

random2DCluster :: [Double] -> [Double] -> [(Double, Double)] -> ([[Double]], [[Double]])
random2DCluster values sigmas xymus = (xs, ys)
  where 
    nClusters = realToFrac (length sigmas) :: Double
    ptsPerCluster = floor ((realToFrac $ length values) / (2.0 * nClusters))
    chunks = chunk ptsPerCluster values
    xs = L.map (\i -> reposition (chunks !! (2*i)) (sigmas !! i)
                     ((L.map fst xymus) !! i)) $ [0..(floor nClusters -1 )]
    ys = L.map (\i -> reposition (chunks !! (2*i + 1)) (sigmas !! i)
                     ((L.map snd xymus) !! i)) $ [0..(floor nClusters -1 )]
    chunk n [] = []
    chunk n list = take n list : chunk n (drop n list)
