--current amount
ones = 10
twos = 10
fives = 15
tens = 20
twenties = 1
fifties = 4
hundreds = 5
five_hundreds = 2
one_thousands = 11

--Available

available_q fig remaining quantity =
	if (remaining `div` fig) > quantity then
		quantity
	else
		remaining `div` fig

available_r fig remaining quantity =
	if (remaining `div` fig) > quantity then
			remaining-(quantity * fig)
		else
			remaining `rem` fig


money n =
	
	if n > 0 then
		show (thousand) ++ " = one thousand rupees notes, \n" ++ 
		show (five_hundred) ++ " = five hundred rupees notes, \n" ++
		show (hundred) ++ " = one hundred rupees notes,\n" ++
		show (fivety) ++ " = fivety rupees notes,\n" ++
		show (twenty) ++ " = twenty rupees notes,\n"++
		show (ten) ++ " = ten rupees notes,\n" ++
		show (five) ++ " = five rupees coins,\n" ++
		show (two) ++ " = two rupees conis,\n" ++
		show (one) ++ " = one rupees conis.\n"

	else
		"money should be greater than zero.\n"

	where
		thousand =(available_q 1000 n one_thousands)
		thousand_rem =(available_r 1000 n one_thousands)

		five_hundred =(available_q 500 thousand_rem five_hundreds)
		five_hundred_rem = (available_r 500 thousand_rem five_hundreds)

		hundred = (available_q 100 five_hundred_rem hundreds)
		hundred_rem = (available_r 100 five_hundred_rem hundreds)
		
		fivety = (available_q 50 hundred_rem fifties)
		fivety_rem = (available_r 50 hundred_rem fifties)

		twenty = (available_q 20 fivety_rem twenties)
		twenty_rem = (available_r 20 fivety_rem twenties)

		ten = (available_q 10 twenty_rem tens)
		ten_rem = (available_r 10 twenty_rem tens)

		five = (available_q 5 ten_rem fives)
		five_rem = (available_r 5 ten_rem fives)

		two = (available_q 2 five_rem twos)
		two_rem = (available_r 2 five_rem twos)

		one = (available_q 1 two_rem ones)

denominations n = putStr (money n)
