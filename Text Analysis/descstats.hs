module DescStats where

import Data.List

median xs = (/2) $ (ys !! (floor $ fromIntegral(length ys) /2)) +
                   (ys !! (ceiling $ fromIntegral(length ys) /2))
                  where ys = sort xs

mean xs = realToFrac(sum  xs)/fromIntegral(length xs)
--real includes both Fractional and Integral

covariance xs ys = mean $ zipWith (\x y -> (x - (mean xs))*(y - (mean ys))) xs ys

variance xs = covariance xs xs

pearsonr xs ys = (covariance xs ys) / (stddev' xs) / (stddev' ys)

mode xs =
  let table = tableCount [] xs
  in mean (getWithCount [] (largest 0 (map snd table)) table)

-- Element a is key, value is count
tableCount :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
tableCount acc [] = acc
tableCount acc (x:xs) = tableCount (tabler acc x) xs

tabler :: Eq a => [(a, Int)] -> a -> [(a, Int)]
tabler [] x = [(x, 1)]
tabler (y:ys) x
  | (fst y) == x = (fst y, snd y + 1) : ys
  | otherwise    = y : (tabler ys x)

largest :: Ord a => a -> [a] -> a
largest acc [] = acc
largest acc (x:xs)
  | x >= acc  = largest x xs
  | otherwise = largest acc xs

getWithCount acc n [] = acc
getWithCount acc n (x:xs)
  | n == snd x = getWithCount ((fst x):acc) n xs
  | otherwise  = getWithCount acc n xs

stddev xs = (**0.5) $ mean (map (**2) xs) - (mean xs)**2

range xs = (minimum xs, maximum xs)

-- Briefer implementations --
----------------------------------------------------------------
mode' xs =
  let table = tableCount' xs; n = largest' (map snd table)
  in (mean $ getWithCount' n table, n)

tableCount' xs = foldl tabler [] xs

largest' xs = foldr (\a b -> if a >= b then a else b) 0 xs

getWithCount' n xs =
  foldr (\x -> if (snd x == n) then ([fst x] ++) else ([] ++)) [] xs

stddev' xs = sqrt $ variance xs

