{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-alberick.mahoussi
-- File description:
-- ImgCompressor
-}

module Parser
  ( parseInputFile
  ) where

import Types (Pixel(..), Color(..))
import Control.Monad (mapM)
import Text.Read (readMaybe)

parseInputFile :: FilePath -> IO (Either String [Pixel])
parseInputFile filePath = do
    content <- readFile filePath
    let linesContent = lines content
    parsedPixels <- mapM parsePixel linesContent
    return $ sequence parsedPixels

parsePixel :: String -> IO (Either String Pixel)
parsePixel line = 
    let parts = words line
    in case parts of
        [pointStr, colorStr] ->
            let parsedPoint = parsePoint pointStr
                parsedColor = parseColor colorStr
            in case (parsedPoint, parsedColor) of
                (Just (x, y), Just c) -> return $ Right $ Pixel x y c
                _ -> return $ Left $ "Error parsing pixel: " ++ line
        _ -> return $ Left $ "Error parsing pixel: " ++ line

parsePoint :: String -> Maybe (Int, Int)
parsePoint str =
    case splitOn "," (init (tail str)) of
      [xStr, yStr] -> (,) <$> readMaybe xStr <*> readMaybe yStr
      _ -> Nothing

parseColor :: String -> Maybe Color
parseColor str =
    case splitOn "," (init (tail str)) of
      [rStr, gStr, bStr] -> 
        Color <$> readMaybe rStr <*> readMaybe gStr <*> readMaybe bStr
      _ -> Nothing

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim str = case breakList delim str of
  (x, []) -> [x]
  (x, _:ys) -> x : splitOn delim ys

breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList [] xs = ([], xs)
breakList _ [] = ([], [])
breakList delim xs@(x:xs')
  | startsWith delim xs = ([], xs)
  | otherwise = let (ys, zs) = breakList delim xs' in (x:ys, zs)

spanList :: (a -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([], [])
spanList p list@(x:xs)
  | p x = let (ys, zs) = spanList p xs in (x:ys, zs)
  | otherwise = ([], list)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys
