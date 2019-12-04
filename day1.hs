module Day1 where

import Text.Printf

fullInput = "R1, L3, R5, R5, R5, L4, R5, R1, R2, L1, L1, R5, R1, L3, L5, L2, R4, L1, R4, R5, L3, R5, L1, R3, L5, R1, L2, R1, L5, L1, R1, R4, R1, L1, L3, R3, R5, L3, R4, L4, R5, L5, L1, L2, R4, R3, R3, L185, R3, R4, L5, L4, R48, R1, R2, L1, R1, L4, L4, R77, R5, L2, R192, R2, R5, L4, L5, L3, R2, L4, R1, L5, R5, R4, R1, R2, L3, R4, R4, L2, L4, L3, R5, R4, L2, L1, L3, R1, R5, R5, R2, L5, L2, L3, L4, R2, R1, L4, L1, R1, R5, R3, R3, R4, L1, L4, R1, L2, R3, L3, L2, L1, L2, L2, L1, L2, R3, R1, L4, R1, L1, L4, R1, L2, L5, R3, L5, L2, L2, L3, R1, L4, R1, R1, R2, L1, L4, L4, R2, R2, R2, R2, R5, R1, L1, L4, L5, R2, R4, L3, L5, R2, R3, L4, L1, R2, R3, R5, L2, L3, R3, R1, R3"
test1 = "R2, L3"
test2 = "R2, R2, R2, R2"
test3 = "R5, L5, R5, R3"
test4 = "R20, L30"

findBlocks :: String -> Int
findBlocks rawInput = 
    walkRoute 0 0 'N' (parseInput rawInput)

walkRoute :: Int -> Int -> Char -> [String] -> Int
walkRoute xSum ySum facing (direction:directions) =
    let 
        newFacing = findNewFacing facing (head direction)
        distance = findDistance facing (head direction) (read (tail direction) :: Int)
    in 
        case facing of 
            'N' -> walkRoute (xSum + distance) ySum newFacing directions
            'S' -> walkRoute (xSum + distance) ySum newFacing directions
            'E' -> walkRoute xSum (ySum + distance) newFacing directions
            'W' -> walkRoute xSum (ySum + distance) newFacing directions
            _ -> error "invalid facing in walkRoute"
walkRoute xSum ySum _ [] =
    (abs xSum) + (abs ySum)

findNewFacing :: Char -> Char -> Char
findNewFacing 'N' 'L' = 'W'
findNewFacing 'N' 'R' = 'E'
findNewFacing 'E' 'L' = 'N'
findNewFacing 'E' 'R' = 'S'
findNewFacing 'S' 'L' = 'E'
findNewFacing 'S' 'R' = 'W'
findNewFacing 'W' 'L' = 'S'
findNewFacing 'W' 'R' = 'N'
findNewFacing _ _ = error "Invalid facing/direction in findNewFacing"

findDistance :: Char -> Char -> Int -> Int
findDistance 'N' 'L' distance = negate distance
findDistance 'N' 'R' distance = distance
findDistance 'E' 'L' distance = distance
findDistance 'E' 'R' distance = negate distance
findDistance 'S' 'L' distance = distance
findDistance 'S' 'R' distance = negate distance
findDistance 'W' 'L' distance = negate distance
findDistance 'W' 'R' distance = distance
findDistance x y z = error $ printf "Invalid facing/direction in findDistance: %c %c %d" x y z

parseInput :: String -> [String]
parseInput input =
    words $ filter (/=',') input 
