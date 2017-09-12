import System.Random
import System.Random.Shuffle
import System.Directory
import Graphics.EasyPlot
import Data.List as L

import ClusteringTools

{-
Kmeans is an algorithm of balance.
Hierarchical clustering scales badly: ~ n^2 comparisons * n/(# categories) iterations. 
-}

main = do
  g <- newStdGen
  let nClusters = 8                               -- User tunable parameters
      values = take 500 $ randoms g :: [Double]
      randVals = L.map ((*2).((-)0.5)) $ take (nClusters * 2) $ randoms g
      planePoints = zip (take nClusters randVals) (drop nClusters randVals)
      sigmas = take nClusters $ randomRs (0.2, 0.3) g :: [Double]
      clusters = random2DCluster values sigmas planePoints
      points = (\(a, b) -> zip (concat a) (concat b)) $ clusters
      points' = shuffle' points (length points) g
  
  let randVals' = take 50 $ randomRs (-0.1, 0.1) g :: [Double]
      circledPoints =
        L.map (\(x, y)->
                 L.map (\g -> (x + g, y + sqrt (0.01 - g^2))) randVals' ++
                 L.map (\g -> (x + g, y - sqrt (0.01 - g^2))) randVals' )
                planePoints
  
  hierarchicalIO points' circledPoints
  kmeansIO points' circledPoints nClusters

kmeansIO points' circledPoints nClusters = do
  g <- newStdGen
  let labels = take 1000 $ randomRs (1, nClusters * 5) g :: [Int]
      data' = zip labels points'
      data'' = L.map snd $ group' $ runKmeans data' (improveKmeans data')

  plot' [Interactive] Windows $
    (L.map (Data2D [Style Dots] []) circledPoints) ++ (L.map (Data2D [] []) data'')
    
  temp <- getLine -- Allows user to interrupt (long waiting time for jpg output)
  createDirectoryIfMissing False "Output Kmeans"
  outputKmeans data' (improveKmeans data') 0
  return ()

hierarchicalIO points' circledPoints = do
  let data' = zip [1..(length points')] (L.map (:[]) points')
      data'' = L.map snd $ runClosestCentroids 8 data'

  plot' [Interactive] Windows $
    (L.map (Data2D [Style Dots] []) circledPoints) ++ (L.map (Data2D [] []) data'')
