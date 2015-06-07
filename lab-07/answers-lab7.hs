--Q1

isLog xs= or [x | x <-xs , x==True]
--or
log_ xs = or (xs)

--Q2

con xss = [x |xs<-xss,x<-xs]

--Q3

iden 0 xs = []

iden n num = num: iden y num
	where y = n - 1 

--Q4

num xs n = head [x | (x,y) <-zip xs ys,y==n]
	where ys=[0..n]

--Q5

isIn xs e = or [x==e|x<-xs]


--Q6
merge_l [] x = x
merge_l x [] = x
merge_l (x:xs) (y:ys) = x:y: merge_l xs ys 


--Q7
isSpace x = x==' '

--Q8

myAny xs cr = or [cr x| x<-xs]
my2 xs cr = or (map cr xs)
my3 xs cr = filter cr xs /= []
