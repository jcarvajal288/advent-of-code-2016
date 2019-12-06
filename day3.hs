module Day3 where

import System.IO

getTriangles :: FilePath -> IO [String]
getTriangles path = do
    contents <- readFile path
    let triangles = map read . lines $ contents
    return triangles

countValidTriangles :: IO Int
countValidTriangles = do
    triangles <- getTriangles "day3input"
    return $ length $ filter isValidTriangle triangles

isValidTriangle :: String -> Bool
isValidTriangle triangleStr = 
    let 
        triangle = map (read::String->Int) $ words triangleStr
    in
        and [testFirst triangle, testSecond triangle, testThird triangle]
    where
        testFirst t = (head t) < (sum $ tail t)
        testSecond t = (head $ tail t) < (sum [head t, last t])
        testThird t = (last t) < (sum $ init t)


main :: IO ()
main = do
    numTriangles <- countValidTriangles
    putStrLn $ show numTriangles
