intSqrt :: Int -> Int
intSqrt = ceiling . (sqrt :: Double -> Double) . fromIntegral

isInt :: Double -> Bool
isInt n = n == fromInteger (floor n)

findYK :: Int -> Int -> (Int, Int)
findYK num sm = go num sm 0
  where
    go n s k
      | isInt sqrtY = (truncate sqrtY, k)
      | otherwise = go n s (k+1)
      where
        y = (s + k) ^ (2 :: Int) - n
        sqrtY = sqrt $ fromIntegral y

findFractions :: Int -> [Int]
findFractions n = [x+y, x-y]
  where
    s = intSqrt n
    yk = findYK n s
    y = fst yk
    k = snd yk
    x = s+k

euler3 :: Int -> Int
euler3 n
  | a == 1 || b == 1 = n
  | otherwise = maximum [euler3 a, euler3 b]
  where
    fs = findFractions n
    a = head fs
    b = fs!!1

main :: IO ()
main = print $ euler3 600851475143
