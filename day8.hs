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

main :: IO ()
main = do
    let screen = createBlankScreen
    printScreen screen
