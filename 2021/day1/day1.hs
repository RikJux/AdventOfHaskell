import System.IO 
import Data.List  
    
main = do     
    contents <- readFile "input.txt" 
    let mes = map (\x -> read x :: Int) $ lines contents

    -- part 1
    print $ countIncreases mes

    -- part 2
    let [shiftedBy2, shiftedBy1, base] = slidingWindowPrep 3 mes
    let compMes = zipWith3 (\x y z -> x+y+z) base shiftedBy1 shiftedBy2

    print $ countIncreases compMes

countIncreases :: [Int] -> Int
countIncreases measurements = 
    let [shifted, base] = slidingWindowPrep 2 measurements
    in length $ filter (\(x, y) -> x < y) $ zip base shifted

slidingWindowPrep :: Int -> [Int] -> [[Int]]
slidingWindowPrep _ [] = []
slidingWindowPrep 1 xs = [xs]
slidingWindowPrep winSize xs = (drop nDrop xs):(slidingWindowPrep nDrop xs)
    where nDrop = winSize-1
