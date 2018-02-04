(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* problem 1-a *)
fun all_except_option (elem, list) =
    let fun exist (elem, list) =
	    case list of
		[] => false
	      | hd::ls => if same_string (elem, hd) then true else exist (elem, ls)
	fun except (elem, list) =
	    case list of
		[] => []
	      | hd::ls => if same_string (elem, hd) then except (elem, ls) else hd::except(elem, ls)
    in if exist (elem, list)
       then SOME ( except (elem, list) )
       else NONE
    end

(* problem 1-b *)
fun get_substitutions1 (lls , elem) =
    case lls of
	[] => []
      | head::tail => let val opt = all_except_option (elem , head)
		  in if isSome opt then valOf (opt) @ get_substitutions1 (tail , elem)
		     else get_substitutions1 (tail , elem)
		  end
(* problem 1-c *)
fun get_substitutions2 (lls, elem) =
    let fun aux (lls , elem , ans) =
	    case lls of
		[] => ans
	      | head::tail => let val opt = all_except_option (elem , head)
			  in if isSome (opt) then aux (tail , elem , ans @ valOf (opt))
			     else aux (tail , elem , ans)
			  end
    in aux (lls , elem , [])
    end

(* problem 1-d *)
type full_name = {first : string , middle : string , last : string}
		     
fun similar_names (lls , name) =
    let val {first=f,middle=m,last=l} = name
	val ls = get_substitutions2 (lls , f)
	fun sub_in (ls) =
	    case ls of
		[] => []
	      | head::tail => {first=head,middle=m,last=l} :: sub_in(tail)
    in [name] @ sub_in (ls)
    end
							  
(* You may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
(* put your solutions for problem 2 here *)
(* a *)
fun card_color (st , rk) =
    case st of (Clubs | Spades) => Black
	     | (Diamonds | Hearts) => Red
					  
(* b *)
fun card_value (st , rk) =
    case rk of (Jack | Queen | King) => 10
	     | Ace => 11
	     | Num i => i
			    
(* c *)
fun remove_card (cs , c , e) =
    let fun exist (cs,c) =
	    case cs of [] => false
		     | hd::tl => if c = hd then true else exist (tl,c)
	fun remove_once (cs,c) =
	    case cs of [] => []
		    | hd::tl => if hd = c then tl else hd::remove_once (tl,c)
    in if exist (cs,c) then remove_once (cs,c)
       else raise e
    end
	
(* d *)
fun all_same_color cs =
    case cs of ( [] | [_] ) => true
	     | h1::h2::tl => if card_color h1 = card_color h2
			     then all_same_color (h2::tl)
			     else false

(* e *)
fun sum_cards cs =
    let fun aux (cs , sum) =
	    case cs of [] => sum
		     | hd::tl => aux (tl , sum + card_value hd)
    in aux (cs , 0)
    end
					 
(* f *)
fun score (cs , gl) =
    let val sum = sum_cards cs
	val pr_score = if sum > gl then 3 * (sum - gl) else gl - sum
    in
	if all_same_color cs then pr_score div 2 else pr_score
    end

(* g *)
fun officiate ( card_list , move_list , goal) =
    let fun aux (cards , helds , moves , goal) =
	    case ( cards , helds , moves , goal ) of
		( _ , h , [] , g ) => score ( h , g )
	      | ( [] , h , Draw::_ , g ) => score ( h , g )
	      | ( [] , h , (Discard x)::tl , g ) => aux ( [] , remove_card ( h , x , IllegalMove ) , tl , g)
	      | ( c::tl' , h , m::tl , g ) => case m of
						  Discard x => aux ( c::tl' , remove_card( h , x , IllegalMove ) , tl , g )
					       |  Draw => let val sum = sum_cards h
							  in if sum > g then score ( c::h , g )
							     else aux ( tl' , c::h , tl , g )
							  end		
    in aux ( card_list , [] , move_list , goal )
    end

(* problem 3 challenge *)
fun score_challenge ( cs , gl ) =
    let val same_color = all_same_color cs
	fun aux ( cs , gl , sum ) = 
	    case cs of
		[] =>  let val pr_score = if sum > gl then 3 * (sum - gl) else gl - sum
		       in
			   if same_color then pr_score div 2 else pr_score
		       end
	      | (_,Ace)::tl => Int.min ( aux ( tl , gl , sum + 1) , aux ( tl , gl , sum + 11 ) )
	      | card::tl => aux ( tl , gl , sum + card_value (card) )
    in aux ( cs , gl , 0 )
    end

fun officiate_challenge ( card_list , move_list , goal) =
    let fun aux (cards , helds , moves , goal) =
	    case ( cards , helds , moves , goal ) of
		( _ , h , [] , g ) => score_challenge ( h , g )
	      | ( [] , h , Draw::_ , g ) => score_challenge ( h , g )
	      | ( [] , h , (Discard x)::tl , g ) => aux ( [] , remove_card ( h , x , IllegalMove ) , tl , g)
	      | ( c::tl' , h , m::tl , g ) => case m of
						  Discard x => aux ( c::tl' , remove_card( h , x , IllegalMove ) , tl , g )
					       |  Draw => let fun min_sum cs =
								  case cs of
								      [] => 0
								    | (_,Ace)::tail => min_sum (tail) + 1
								    | card::tail => min_sum(tail) + card_value (card)
							  in if min_sum h > g then score_challenge ( c::h , g )
							     else aux ( tl' , c::h , tl , g )
							  end		
    in aux ( card_list , [] , move_list , goal )
    end
