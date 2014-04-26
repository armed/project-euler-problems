euclid :: Int -> Int -> [Int]
euclid m n = [a, b, c]
  where
    p = 2 :: Int
    m2 = m ^ p
    n2 = n ^ p
    a = m2 - n2
    b = p * m * n
    c = m2 + n2

findTriangle :: [Int]
findTriangle = head [e | m <- [10..20], n <- [5..20],
                      let e = euclid m n,
                      n < m,
                      sum e == 1000]

main :: IO ()
main = print $ product findTriangle
