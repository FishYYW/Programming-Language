fun is_older (d1 : int * int * int , d2 : int * int * int) =
    if #1 d1 < #1 d2
    then true
    else
	if #1 d1 = #1 d2 andalso #2 d1 < #2 d2 
	then true
	else
	    if #1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2
	    then true
	    else false

fun number_in_month (dat : (int * int * int) list , mon : int) =
    if null dat
    then 0
    else
	let val t = if #2 (hd dat) = mon
		    then 1
		    else 0
	in number_in_month (tl dat , mon) + t
	end
	 
fun number_in_months (dat : (int * int * int) list , mon : int list) =
    if null mon
    then 0
    else number_in_month (dat , hd mon) + number_in_months (dat , tl mon)

fun dates_in_month (dat : (int * int * int) list , mon : int) =
    if null dat
    then []
    else
	if #2 (hd dat) = mon
	then (hd dat) :: dates_in_month (tl dat , mon)
	else dates_in_month (tl dat , mon)
			    
fun append_list (xs1 : (int * int * int) list , xs2 : (int * int * int) list) = (* or use @ instead *)
    if null xs1
    then xs2
    else hd xs1 :: append_list (tl xs1 , xs2)
			       
fun dates_in_months (dat : (int * int * int) list , mon : int list) =
    if null mon
    then []
    else append_list (dates_in_month (dat , hd mon) , dates_in_months (dat , tl mon))

fun get_nth (ls : string list , n : int) =
    if n <= 1
    then hd ls
    else get_nth (tl ls , n-1)

fun date_to_string (dat : int * int * int) =
    let val month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(month,#2 dat) ^ " " ^ Int.toString (#3 dat) ^ ", " ^ Int.toString (#1 dat)
    end

fun number_before_reaching_sum (sum : int , xs : int list) =
    if sum <= 0
    then ~1
    else 1 + number_before_reaching_sum (sum - hd xs , tl xs)

fun what_month (day : int) =
    let val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 1 + number_before_reaching_sum (day, days_in_month)
    end

fun month_range (d1 : int , d2 : int) =
    if d1 > d2
    then []
    else
	if d1 = d2
	then [what_month (d2)]
	else what_month (d1) :: month_range (d1 + 1 , d2)

fun oldest (dat : (int * int * int) list) =
    if null dat
    then NONE
    else
	if null (tl dat)
	then SOME (hd dat)
	else let val old = oldest(tl dat)
	     in if is_older(hd dat , valOf (old))
		then SOME (hd dat)
		else old
	     end

			
fun remove_duplicate (xs : int list) =
    let fun if_contain(x : int , xs : int list) =
	    if null xs
	    then false
	    else
		if x = hd xs
		then true
 		else if_contain (x , tl xs)
	fun go_through (xs : int list) =
	    if null xs
	    then []
	    else
		if if_contain (hd xs, tl xs)
		then go_through (tl xs)
		else hd xs :: go_through (tl xs)
    in go_through (xs)
    end

fun number_in_months_challenge (dat : (int * int * int) list , mon : int list) =
    number_in_months (dat , remove_duplicate (mon))

fun dates_in_months_challenge (dat : (int * int * int) list , mon : int list) =
    dates_in_months (dat , remove_duplicate (mon))

fun reasonable_date (dat : int * int * int) =
    let val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
	val days_in_month_leap = [31,29,31,30,31,30,31,31,30,31,30,31]
	fun leap (year : int) =
	    year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
	fun nth_int (xs : int list , n : int) =
	    if n <= 1
	    then hd xs
	    else nth_int (tl xs , n-1)
    in
	if #1 dat <= 0
	then false
	else if #2 dat  < 1 orelse #2 dat > 12
	then false
	else
	    if leap (#1 dat)
	    then nth_int (days_in_month_leap , #2 dat) >= #3 dat
	    else nth_int (days_in_month , #2 dat) >= #3 dat
    end
