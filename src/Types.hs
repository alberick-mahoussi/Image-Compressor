{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-alberick.mahoussi
-- File description:
-- ImgCompressor
-}

module Types
  ( Pixel (..)
  , Color (..)
  , Cluster (..)
  , distance
  ) where

data Pixel = Pixel
  { x :: Int
  , y :: Int
  , color :: Color
  } deriving (Show, Eq)

data Color = Color
  { red :: Int
  , green :: Int
  , blue :: Int
  } deriving (Show, Eq)

data Cluster = Cluster
  { centroid :: Color
  , members :: [Pixel]
  } deriving (Show, Eq)

distance :: Color -> Color -> Float
distance c1 c2 = 
  sqrt $ fromIntegral $ squaredDiff red + squaredDiff green + squaredDiff blue
  where
    squaredDiff f = (f c1 - f c2) ^ (2 :: Int)
