{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-alberick.mahoussi
-- File description:
-- ImgCompressor
-}

module Display
    ( printClusters
    ) where

import Types

displayClusterHeader :: Color -> IO ()
displayClusterHeader (Color r g b) = 
    putStrLn $ "--\n(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")\n-"

displayPixel :: Pixel -> IO ()
displayPixel (Pixel x y (Color r g b)) = 
    putStrLn $ "(" ++ show x ++ "," ++ show y ++ ") (" ++ show r 
    ++ "," ++ show g ++ "," ++ show b ++ ")"

displayCluster :: Cluster -> IO ()
displayCluster (Cluster meanColor pixels) = 
    displayClusterHeader meanColor >>
    mapM_ displayPixel pixels


printClusters :: [Cluster] -> IO ()
printClusters clusters = mapM_ displayCluster clusters