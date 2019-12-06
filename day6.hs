module Day6 where

import Data.Text (unpack)
import Data.Text.IO (readFile)
import Prelude hiding (readFile, lookup)
import Data.Map.Strict hiding (map)
import Data.List (elemIndices, nub, transpose)

getRows :: FilePath -> IO [String]
getRows path = do
    contents <- readFile path
    let rows = lines $ unpack $ contents
    return rows

findMostCommonLetter :: String -> Char
findMostCommonLetter message = 
    snd $ head $ assocs hashMap
    where hashMap = fromList $ map (\x -> (length (elemIndices x message), x)) (nub message) 

main :: IO ()
main = do
    rows <- getRows "day6input"
    let mostCommonLetters = map findMostCommonLetter $ transpose rows
    putStrLn mostCommonLetters
