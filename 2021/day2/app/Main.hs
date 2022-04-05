module Main where

main :: IO ()
main = do
    inp <- readFile "/mnt/Files/Dev/Haskell/adventOfCode/input.txt"
    print $ checkDirections (map words $ lines inp) (0, 0, 0)

checkDirections :: [[String]] -> (Int, Int, Int) -> Int
checkDirections (["forward", steps]:xs) (horiz, aim, depth) = checkDirections xs (horiz + read steps, aim             , depth + aim * read steps)
checkDirections (["down"   , steps]:xs) (horiz, aim, depth) = checkDirections xs (horiz             , aim + read steps, depth                   )
checkDirections (["up"     , steps]:xs) (horiz, aim, depth) = checkDirections xs (horiz             , aim - read steps, depth                   )
checkDirections _                       (horiz,   _, depth) = horiz * depth