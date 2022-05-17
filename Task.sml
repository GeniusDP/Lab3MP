(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* put your solutions for problem 1 here *)
    
    
    (* PROBLEM 1A *)
fun all_except_option (str, sl) =
  case sl of [] => NONE | x::xs => case same_string(str, x) of
        true => SOME(xs) | false => case all_except_option(str, xs) of
            NONE => NONE | SOME y => SOME(x::y);
  

fun provided_test1 () = 
    let val elem = "zaranik"
        val array = ["zaranik"]
    in
        all_except_option (elem, array) = SOME []
    end;

fun provided_test2 () = 
    let val elem = "zaranik"
        val array = ["bogdan"]
    in
        all_except_option (elem, array) = NONE
    end;

fun provided_test3 () = 
    let val elem = "zaranik"
        val array = ["zaranik", "barinov", "bazukin"]
    in
        all_except_option (elem, array) = SOME ["barinov", "bazukin"]
    end;

fun provided_test4 () = 
    let val elem = "zaranik"
        val array = ["barinov", "zaranik", "bazukin"]
    in
        all_except_option (elem, array) = SOME ["barinov", "bazukin"]
    end;

fun provided_test5 () = 
    let val elem = "zaranik"
        val array = ["bazukin", "barinov", "zaranik"]
    in
        all_except_option (elem, array) = SOME ["bazukin", "barinov"]
    end;


val ALL_EXCEPT_OPTION_1 = provided_test1 ()
val ALL_EXCEPT_OPTION_2 = provided_test2 ()
val ALL_EXCEPT_OPTION_3 = provided_test3 ()
val ALL_EXCEPT_OPTION_4 = provided_test4 ()
val ALL_EXCEPT_OPTION_5 = provided_test5 ()



    (* PROBLEM 1B *)

fun get_substitutions1 (xss, y) =
    case xss of
        [] => [] | xs :: xss' => case all_except_option(y, xs) of
            NONE => get_substitutions1(xss', y) | SOME z => z @ get_substitutions1(xss', y);

fun provided_test1 () = 
    let val array = [["foo"],["there"]]
        val elem = "foo"
    in
        get_substitutions1(array, elem) = []
    end;

fun provided_test2 () = 
    let val array = [["zaranik","nick"],["don","zaranik","kik"]]
        val elem = "zaranik"
    in
        get_substitutions1(array, elem) = ["nick", "don", "kik"]
    end;

fun provided_test3 () = 
    let val array = [["zaranik","nick"],["liza","tania"],["don","zaranik","kik"]]
        val elem = "zaranik"
    in
        get_substitutions1(array, elem) = ["nick","don","kik"]
    end;

fun provided_test4 () = 
    let val array = [["fred","fredrick"],["elizabeth","betty","fred"],["freddie","fred","kik"]]
        val elem = "fred"
    in
        get_substitutions1(array, elem) = ["fredrick","elizabeth","betty","freddie","kik"]
    end;


val GET_SUBSTITUTIONS1_1 = provided_test1 ()
val GET_SUBSTITUTIONS1_2 = provided_test2 ()
val GET_SUBSTITUTIONS1_3 = provided_test3 ()
val GET_SUBSTITUTIONS1_4 = provided_test4 ()


    (*PROBLEM 1C*)
fun get_substitutions2 (variable1, y) =
        let fun helper (helper_ss, acc) =
            case helper_ss of [] => acc
                | hs :: helper_ss' => case all_except_option(y, hs) of
                    NONE => helper(helper_ss', acc) | SOME z => helper(helper_ss', acc @ z)
        in helper(variable1, [])
    end;



fun provided_test1 () = 
    let val array = [["foo"],["there"]]
        val elem = "foo"
    in
        get_substitutions2(array, elem) = []
    end;

fun provided_test2 () = 
    let val array = [["zaranik","nick"],["don","zaranik","kik"]]
        val elem = "zaranik"
    in
        get_substitutions2(array, elem) = ["nick", "don", "kik"]
    end;

fun provided_test3 () = 
    let val array = [["zaranik","nick"],["liza","tania"],["don","zaranik","kik"]]
        val elem = "zaranik"
    in
        get_substitutions2(array, elem) = ["nick","don","kik"]
    end;

fun provided_test4 () = 
    let val array = [["fred","fredrick"],["elizabeth","betty","fred"],["freddie","fred","kik"]]
        val elem = "fred"
    in
        get_substitutions2(array, elem) = ["fredrick","elizabeth","betty","freddie","kik"]
    end;

val GET_SUBSTITUTIONS2_1 = provided_test1 ()
val GET_SUBSTITUTIONS2_2 = provided_test2 ()
val GET_SUBSTITUTIONS2_3 = provided_test3 ()
val GET_SUBSTITUTIONS2_4 = provided_test4 ()


    (*PROBLEM 1D*)
fun similar_names (arrr, {first=f, middle=m, last=l}) =
    let fun function_for_substitution (arr) =
            case arr of [] => [] | x::arr' => {
                first=x, 
                middle=m, 
                last=l
            } :: function_for_substitution(arr')
    in
        {
            first=f, 
            middle=m, 
            last=l
        } :: function_for_substitution( get_substitutions2(arrr, f) )
    end;


fun provided_test1 () = 
    let val array1 = [
            ["Fred","Fredrick"],
            ["Elizabeth","Betty"],
            ["Freddie","Fred","F"]
        ]
        val full_name = {first="Fred", middle="W", last="Smith"}
        val result_array = [
            {first="Fred", last="Smith", middle="W"},
            {first="Fredrick", last="Smith", middle="W"},
	        {first="Freddie", last="Smith", middle="W"}, 
            {first="F", last="Smith", middle="W"}
        ]

    in
        similar_names(array1, full_name) = result_array
    end;



val SIMILAR_NAMES_1 = provided_test1 ();

    
    (* TASK 2 *)



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
    (*PROBLEM 2A*)
fun card_color (suit, rank) = 
  case suit of
    Spades => Black
    | Clubs => Black
    | Diamonds => Red
    | Hearts => Red



fun provided_test1 () = 
    let val var1 = Clubs
        val var2 = Num 2
    in
        card_color(var1, var2) = Black
    end;

fun provided_test2 () = 
    let val var1 = Spades
        val var2 = Num 2
    in
        card_color(var1, var2) = Black
    end;

fun provided_test3 () = 
    let val var1 = Diamonds
        val var2 = Num 2
    in
        card_color(var1, var2) = Red
    end;

fun provided_test4 () = 
    let val var1 = Hearts
        val var2 = Num 2
    in
        card_color(var1, var2) = Red
    end;

val CARD_COLOUR_TEST_1 = provided_test1()
val CARD_COLOUR_TEST_2 = provided_test2()
val CARD_COLOUR_TEST_3 = provided_test3()
val CARD_COLOUR_TEST_4 = provided_test4()



    (*PROBLEM 2B*)
fun card_value (suit, rank) =
  case rank of
    Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num i => i


fun provided_test1 () = 
    let val var1 = Clubs
        val var2 = Num 2
    in
        card_value(var1, var2) = 2
    end;

fun provided_test2 () = 
    let val var1 = Clubs
        val var2 = Ace
    in
        card_value(var1, var2) = 11
    end;

fun provided_test3 () = 
    let val var1 = Clubs
        val var2 = King
    in
        card_value(var1, var2) = 10
    end;

val CARD_VALUE_TEST_1 = provided_test1()
val CARD_VALUE_TEST_2 = provided_test2()
val CARD_VALUE_TEST_3 = provided_test3()