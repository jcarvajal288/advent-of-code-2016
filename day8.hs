module Day8 where

createBlankScreen :: [String]
createBlankScreen =
    ["..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     ".................................................."]

printScreen :: [String] -> IO ()
printScreen screen = do
    putStrLn $ unlines screen

rect :: Int -> Int -> [String] -> [String]
rect a b screen =
    let 
        newRow = ['#' if x<=a else '.' | <- x [1..50]]
    in
        blitRow
            

main :: IO ()
main = do
    let screen = createBlankScreen
    printScreen screen
