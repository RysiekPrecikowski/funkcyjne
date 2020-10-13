sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
            then 0
            else 1


--zadania

absInt :: Int -> Int --absInt 2 = absInt (-2) = 2
absInt n = if n < 0
            then -n
            else n

min2Int :: (Int, Int) -> Int -- min (1,2) = 1, min (-1, -1) = -1
min2Int (a,b) = if a < b
                then a
                else b

min3Int :: (Int, Int, Int) -> Int
min3Int (a,b,c) = if  a < b && a < c
                    then a
                    else if b < a && b < c 
                        then b
                    else 
                        c


min3IntF :: (Int, Int, Int) -> Int
min3IntF (a,b,c) = min2Int(min2Int(a,b), min2Int(b,c))

--jeszcza sa opcjonalne (od 5)