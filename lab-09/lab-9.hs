import Data.List.Split
palin xs  = reverse xs == xs

---
myR a b xs = [(x,y) | (x,y)<-xs, not (a==x && b==y)] 

sear_x xs x = not (or [x==x'|(x',y)<-xs])

sear_y xs y = not (or [y==y'|(x,y')<-xs])

dia sum xs = not (or [sum==(x+y)|(x,y)<-xs] )

quee xs = [(x,y) | (x,y)<-xs , sear_x (myR x y xs) x, sear_y (myR x y xs) y, dia (x+y) (myR x y xs)]

queen xs = quee xs == xs

--
isSpace x = x == ' '

split_string :: String -> [(Int, String)]
split_string = go 0
  where
    go n s = case break (not . isSpace) s of
      (_, "")  -> []
      (ws, s') -> (n', w) : go (n' + length w) s''
                     where
                       n'       = n + length ws
                       (w, s'') = break isSpace s'

str_position string' word' = [n | (n,s_word)<- (split_string string'),s_word == word']
