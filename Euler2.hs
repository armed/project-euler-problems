euler2 :: Int
euler2 = sum [ i | i <- takeWhile (< limit) fibs, even i]
  where
    limit = 4000000
    fibs = (0 :: Int) : 1 : next fibs
      where
        next (a : t@(b:_)) = (a+b) : next t
