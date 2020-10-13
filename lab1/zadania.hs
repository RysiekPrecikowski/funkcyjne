-- 4
vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (a, c) = (c, a)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x,y,z) = (x == y) && (y==z)

areaHeron :: (Double, Double, Double) -> Double
areaHeron (a,b,c) = sqrt((a+b+c)*(a+b-c)*(a-b+c)*(-a+b+c))/4



--5
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



--6
sgn :: Int -> Int
sgn n | n > 0 = 1
      | n < 0 = -1
      | otherwise = 0


min3Int' :: (Int, Int, Int) -> Int -- min (1,2,3)=1, min (1,1,3)=1
min3Int' (a,b,c) 
    | a < b && a < c = a
    | b < a && b < c = b
    | otherwise = c



--7
or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' _              = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' _            = False

nand' :: (Bool, Bool) -> Bool
nand' (True, True) = False
nand' _            = True

xor' :: (Bool, Bool) -> Bool
xor' (True, True) = False
xor' (False, False) = False
xor' _              = True



--8
isItTheAnswer :: String -> Bool
isItTheAnswer (s) = case (s) of
    "Love" -> True -- :)
    _      -> False

or'' :: (Bool, Bool) -> Bool

or'' (a,b) = case (a,b) of
    (False, False) -> False
    _               -> True
             
and'' :: (Bool, Bool) -> Bool
and'' (a,b) = case (a,b) of
    (True, True) -> True
    _            -> False

nand'' :: (Bool, Bool) -> Bool
nand'' (a,b) = case (a,b) of
    (True, True) -> False
    _            -> True

xor'' :: (Bool, Bool) -> Bool
xor'' (a,b) = case (a,b) of
    (True, True) -> False
    (False, False) -> False
    _              -> True



--9
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a,b) = (a/l, b/l)
    where l = sqrt(a^2 + b^2)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a,b,c) = (a/l, b/l, c/l)
    where l = sqrt(a^2 + b^2 + c^2)

areaHeron' :: (Double, Double, Double) -> (Double)
areaHeron' (a,b,c) = sqrt(p*(p-a)*(p-b)*(p-c))
    where p = (a+b+c)/2



--10
unitVec2D' :: (Double, Double) -> (Double, Double)
unitVec2D' (a,b) = 
    let l = sqrt(a^2 + b^2)
    in (a/l, b/l)

unitVec3D' :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D' (a,b,c) = 
    let l = sqrt(a^2 + b^2 + c^2)
    in (a/l, b/l, c/l)

areaHeron'' :: (Double, Double, Double) -> (Double)
areaHeron'' (a,b,c) = 
    let p = (a+b+c)/2
    in sqrt(p*(p-a)*(p-b)*(p-c))



--11
roots' :: (Double, Double, Double) -> (Double, Double)
roots' (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where {
    d = sqrt (b * b - 4 * a * c);
       e = 2 * a; -- uwaga na przesunięcie!
   }

{-
    wieloliniowy
    komentarz :)
-}

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
 let {
     d = sqrt (b * b - 4 * a * c);
     e = 2 * a;
 }
 in ( (-b - d) / e, (-b + d) / e )