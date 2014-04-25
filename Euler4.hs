largestPalindrome :: Int
largestPalindrome =
  maximum [read pal :: Int |
            a <- [900..999 :: Int],
            b <- [999,998..900 :: Int],
            let pal = show (a*b),
              pal == reverse pal]
