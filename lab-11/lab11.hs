--1a
encode x = fromEnum x - 96
encod x = (fromEnum x) - (fromEnum 'a') + 1
--1b
decod x =  toEnum (96 + x)::Char
decode x = toEnum (x + fromEnum 'a' - 1)::Char

--2
count x xs = sum [n | xa <- xs, xa ==x ]
	where n = 1

isEx x i xs = [ i' | (x',i') <- zip xs [1..n], i'< i,x==x']
	where 
		n = length xs

frequency xs = [ count x xs | (x,i) <- zip xs [1..n] , (isEx x i xs)==[]]
	where 
		n = length xs
