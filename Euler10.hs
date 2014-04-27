isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

primes :: [Int]
primes = 2:3:[c | k <- [0..], r <- [1,5],
            let
              c = 6*k+r
              q = isqrt c
              ps = takeWhile (<=q) primes,
            c > 1,
            all (\x -> c `rem` x /= 0) ps]

main :: IO ()
main = print $ sum $ takeWhile (<2000000) primes
