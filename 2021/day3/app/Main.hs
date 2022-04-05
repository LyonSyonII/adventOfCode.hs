module Main where
import Data.List

main :: IO ()
main = do
    inp <- readFile "/mnt/Files/Dev/Haskell/adventOfCode/input.txt"
    let rows = reverse $ transpose $ lines inp
    print $ calculateConsumption rows 0 0 0

mostFreq :: String -> (Int, Int) -> Bool
mostFreq ('0':xs) (zero, one) = mostFreq xs (zero+1, one)
mostFreq ('1':xs) (zero, one) = mostFreq xs (zero, one+1)
mostFreq _ (zero, one)
 | zero > one = False
 | otherwise = True

calculateConsumption :: [String] -> Int -> Int -> Int -> Int
calculateConsumption (x:xs) i gamma epsilon = let (bit, notBit) = do 
                                                    let b = mostFreq x (0, 0) 
                                                    (fromEnum b, fromEnum $ not b)
                                              in calculateConsumption xs (i+1) (gamma + bit * (2 ^ i)) (epsilon + notBit * (2 ^ i))
calculateConsumption _ _ gamma epsilon = gamma * epsilon
                                                