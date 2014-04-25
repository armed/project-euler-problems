euler1 :: Int -> Int
euler1 n = sum [ i | i <- [3..(n-1)], mod i 3 == 0 || mod i 5 == 0 ]
