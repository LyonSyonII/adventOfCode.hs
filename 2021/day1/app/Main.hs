module Main where

import Lib

main :: IO ()
main = do
    input <- map read . words <$> readFile "/mnt/Files/Dev/Haskell/adventOfCode/input.txt" :: IO [Int]
    print $ countIncreased input 0
    print $ increasingWindows input 0


countIncreased :: [Int] -> Int -> Int
countIncreased (x:xs) count = countIncreased xs count + fromEnum(x < head xs)
countIncreased _ count = count

increasingWindows :: [Int] -> Int -> Int
increasingWindows (x:y:z:a:xs) count = increasingWindows (y:z:a:xs) count + fromEnum(x+y+z < y+z+a)
increasingWindows _ count = count