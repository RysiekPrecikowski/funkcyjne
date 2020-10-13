absInt :: Int -> Int
absInt n | n >= 0 = n
         | otherwise = -n

--zadania
sgn :: Int -> Int
sgn n | n > 0 = 1
      | n < 0 = -1
      | otherwise = 0


min3Int :: (Int, Int, Int) -> Int -- min (1,2,3)=1, min (1,1,3)=1
min3Int (a,b,c) 
    | a < b && a < c = a
    | b < a && b < c = b
    | otherwise = c