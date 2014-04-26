import Data.List
import Data.Maybe

prime :: Int -> Int
prime n = last $ 1 : primes [2, 3, 5 :: Int] (n-3)

primes :: [Int] -> Int -> [Int]
primes ps limit
  | limit <= 0 = ps
  | otherwise = primes (ps ++ [p]) (limit - 1)
  where
    isPrime n = all (\x -> n `rem` x /= 0)
    l = last ps
    xs = l:[l+2,l+4..]
    p = fromJust $ find (`isPrime` ps) xs

main :: IO ()
main = print $ prime 10001
