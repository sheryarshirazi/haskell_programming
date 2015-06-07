sort :: Ord a => [a] -> [a]
sort []         =  []
sort [x]        =  [x]
sort xs         =  merge (sort ys) (sort zs)
  where (ys,zs) = split xs
 
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
      | x<=y      = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
 
split []        =  ([], [])
split [x]       =  ([x], [])
split (x:y:xs)  =  (x:l, y:r)
  where (l, r)  =  split xs

--02
isSpace :: Char ->Bool
isSpace x = x==' '

--03
myAny xs cr = or (map cr xs)
--or
myAny2 xs cr = or ([(cr x) | x<-xs])

--04
anySpace xs = or ([(isSpace x) | x<-xs])

--05
alphNum a = if a >= '0' && a<= '9' || a >= 'A' && a<= 'Z' || a >= 'a' && a<= 'z' then True else False

--06
--foldr for reverse is not applicable

pro [] = 1
pro (x:xs) = x* pro xs

len [] = 0
len (x:xs) = 1 + len xs

--07
remx color xs = [x | x <-xs,not (x==color)]