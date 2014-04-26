-- Oh my God! NATURAL numbers not PRIME numbers
--import Data.List
--import Data.Maybe
--primes :: [Int] -> Int -> [Int]
--primes ps limit
--  | limit <= 0 = ps
--  | otherwise = primes (ps ++ [p]) (limit - 1)
--  where
--    isPrime n = all (\x -> n `rem` x /= 0)
--    l = last ps
--    xs = l:[l+2,l+4..]
--    p = fromJust $ find (`isPrime` ps) xs

main :: IO ()
main = print delta
  where
    p2 = (^(2::Int))
    p100 = [1..100::Int]
    sumPowers = sum $ map p2 p100
    powerSums = p2 $ sum p100
    delta = powerSums - sumPowers
