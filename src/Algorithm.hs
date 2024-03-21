{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-alberick.mahoussi
-- File description:
-- ImgCompressor
-}

module Algorithm
  ( kMeansClustering
  ) where

import Types (Pixel(..), Color(..), Cluster(..))
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Random (randomRIO)
import Control.Monad (forM)
import Data.List (nub)

kMeansClustering :: Int -> Double -> [Pixel] -> IO [Cluster]
kMeansClustering k threshold pixels = do
  initialClusters <- initializeClusters k pixels
  return $ kMeansIteration initialClusters pixels threshold

shuffle :: [a] -> IO [a]
shuffle xs = do
  let n = length xs
  rands <- forM [0..(n-2)] $ \i -> randomRIO (i, n-1)
  return $ swapElements xs rands

swapElements :: [a] -> [Int] -> [a]
swapElements xs [] = xs
swapElements xs (r:rs) = swapElements (swap r (r+1) xs) rs

swap :: Int -> Int -> [a] -> [a]
swap i j xs = let elI = xs !! i
                  elJ = xs !! j
                  left = take i xs
                  middle = take (j - i - 1) (drop (i + 1) xs)
                  right = drop (j + 1) xs
              in left ++ [elJ] ++ middle ++ [elI] ++ right

initializeClusters :: Int -> [Pixel] -> IO [Cluster]
initializeClusters k pixels = do
  uniqueColors <- nub . map color <$> shuffle pixels
  let initialColors = take k uniqueColors
  return $ take k $ zipWith Cluster initialColors (repeat [])

assignPixelsToClusters :: [Cluster] -> [Pixel] -> [Cluster]
assignPixelsToClusters clusters pixels =
  let emptyClusters = map (\(Cluster c _) -> Cluster c []) clusters
      updatedClusters = foldr (\pixel acc ->
        assignPixelToClosestCluster pixel acc) emptyClusters pixels
  in map updateClusterColor updatedClusters

assignPixelToClosestCluster :: Pixel -> [Cluster] -> [Cluster]
assignPixelToClosestCluster pixel clusters =
  let closestClusterIndex = getClosestClusterIndex (color pixel) clusters
  in updateClusterWithPixel pixel closestClusterIndex clusters

getClosestClusterIndex :: Color -> [Cluster] -> Int
getClosestClusterIndex c clusters = 
  fst $ minimumBy (comparing snd) (zip [0..] (map (\(Cluster c' _) -> 
    colorDistance c c') clusters))

updateClusterWithPixel :: Pixel -> Int -> [Cluster] -> [Cluster]
updateClusterWithPixel pixel index clusters =
  let (before, currentCluster:after) = splitAt index clusters
      (Cluster c pixels) = currentCluster
  in before ++ [Cluster c (pixel:pixels)] ++ after

updateClusterColor :: Cluster -> Cluster
updateClusterColor (Cluster _ pixels) =
  let n = length pixels
      meanColor = foldr (\(Pixel _ _ c) (Color r g b) ->
                         let (Color r' g' b') = c
                         in Color (r + r') (g + g') (b + b')
                       ) (Color 0 0 0) pixels
      newColor = Color (red meanColor `div` n) 
       (green meanColor `div` n) (blue meanColor `div` n)
  in Cluster newColor pixels

kMeansIteration :: [Cluster] -> [Pixel] -> Double -> [Cluster]
kMeansIteration clusters pixels threshold
  | hasConverged = clusters
  | otherwise = kMeansIteration newClusters pixels threshold
  where
    newClusters = assignPixelsToClusters clusters pixels
    hasConverged = all (checkConvergence threshold) (zip clusters newClusters)

updateClusters :: [Cluster] -> [Pixel] -> Double -> (Bool, [Cluster])
updateClusters clusters pixels threshold = (hasConverged, newClusters)
  where
    newClusters = assignPixelsToClusters clusters pixels
    hasConverged = all (checkConvergence threshold) (zip clusters newClusters)

addPixelToCluster :: [Pixel] -> Cluster -> Cluster
addPixelToCluster pixels (Cluster c []) = Cluster c (closestPixels c pixels)

closestPixels :: Color -> [Pixel] -> [Pixel]
closestPixels c = filter (\p -> color p == c) 

checkConvergence :: Double -> (Cluster, Cluster) -> Bool
checkConvergence threshold (Cluster c1 pixels1, Cluster c2 pixels2) =
  colorDistance c1 c2 <= threshold

colorDistance :: Color -> Color -> Double
colorDistance (Color r1 g1 b1) (Color r2 g2 b2) = 
  sqrt . fromIntegral $ dr*dr + dg*dg + db*db
  where
    dr = r1 - r2
    dg = g1 - g2
    db = b1 - b2
