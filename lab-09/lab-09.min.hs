import Data.List.Split
--Question 1
palin xs  = reverse xs == xs

--Question 2
myR a b xs = [(x,y) | (x,y)<-xs, not (a==x && b==y)] 

sear_x xs x = not (or [x==x'|(x',y)<-xs])

sear_y xs y = not (or [y==y'|(x,y')<-xs])

dia sum xs = not (or [sum==(x+y)|(x,y)<-xs] )

queen xs = 
	[(x,y) | (x,y)<-xs , sear_x (myR x y xs) x, sear_y (myR x y xs) y, dia (x+y) (myR x y xs)] == xs


--Question 3
{-
	list_E_Lengths:
		this function get list and initial value 0 and return length of elements (must be string)
		if we pass (splitOn " " "some strings here") instead of elements of list (string) then it
		returns substrings with its position
-}

list_E_Lengths [] _ = []
list_E_Lengths (x:xs) n = (n, x): list_E_Lengths xs (n+length x + 1)

strPos sStr str = head [ i | (i,x) <- ( list_E_Lengths (splitOn " " str) 0), sStr == x]

--Question 4

factors n = [x |x<-[1..n], n `mod` x == 0]

prime n = factors n == [1,n]

gold n = head [ (a,b) | a <-[1..n] , b<-[1..n], (a+b)==n, prime a, prime b, even n]