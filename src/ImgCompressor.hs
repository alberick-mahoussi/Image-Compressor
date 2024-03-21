{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-compressor-alberick.mahoussi
-- File description:
-- ImgCompressor
-}

module ImgCompressor
    ( main_imgCompressor
    ) where

import System.Environment (getArgs)
import Parser (parseInputFile)
import Display (printClusters)
import Algorithm (kMeansClustering)

processImage :: Int -> Double -> String -> IO ()
processImage numColors convergenceLimit filePath = do
  parsedResult <- parseInputFile filePath
  case parsedResult of
    Left err -> putStrLn $ "Error: " ++ show err
    Right pixels -> do
      clusters <- kMeansClustering numColors convergenceLimit pixels
      printClusters clusters

parseArgs :: [String] -> Maybe (Int, Double, String)
parseArgs ("-n":v:"-l":x:"-f":i) = goodArgs ("-n":v:"-l":x:"-f":i)
parseArgs _ = Nothing

goodArgs :: [String] -> Maybe (Int, Double, String)
goodArgs args = parseArgsValues (args !! 1) (args !! 3) (args !! 5)

parseArgsValues :: String -> String -> String -> Maybe (Int, Double, String)
parseArgsValues n l f = case (readMaybe n, readMaybe l) of
                          (Just nVal, Just lVal) -> Just (nVal, lVal, f)
                          _ -> Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing

printUsage :: IO ()
printUsage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n\
                      \N: number of colors in the final image\n\
                      \L: convergence limit\n\
                      \F: path to the file containing the colors of the pixels"


main_imgCompressor :: IO ()
main_imgCompressor = do
  args <- getArgs
  case parseArgs args of
    Just (numColors, convergenceLimit, filePath) ->
      processImage numColors convergenceLimit filePath
    Nothing -> printUsage
