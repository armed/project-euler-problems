import Data.List
import Data.Maybe

findLCM :: Maybe Int
findLCM = find pr [2520,(2540::Int)..]
  where
    nums = [3..19]
    pr n = all (\x -> n `mod` x == 0) nums

main :: IO ()
main = print $ fromJust findLCM
