 (* In all problems, a “date” is an SML value of type int*int*int, 
 where the first part is the year, the second part is the month, 
 and the third part is the day. A “reasonable” date has a positive year, 
 a month between 1 and 12, and a day no greater than 31 (or less depending on the month).
 Your solutions need to work correctly only for reasonable dates, 
 but do not check for reasonable dates (that is a challenge problem) 
 and many of your functions will naturally work correctly for some/all non-reasonable dates. 
 A “day of year” is a number from 1 to 365 where, for example, 33 represents February 2. 
 (We ignore leap years except in one challenge problem.) *)

fun is_older (age1: int*int*int, age2:int*int*int) = 
    let 
        fun count_age(age: int*int*int) = 
            12 * 31 * (#1 age) + 31 * (#2 age) + #3 age;
        val age_int_1 = count_age age1;
        val age_int_2 = count_age age2;
    in 
        age_int_1 < age_int_2
    end

val test1 = is_older ((1, 2, 3),(2, 3, 4)) = true
val test1_2 = is_older ((1, 2, 3),(1, 2, 4)) = true
val test1_3 = is_older ((1, 2, 3),(1, 2, 3)) = false
val test1_4 = is_older ((1, 2, 3), (1, 2, 2)) = false

(* TODO: do extra check for this *)
(* fun number_in_month(day_list: (int*int*int) list, real_idx: int) = 
    if real_idx=1
    then #3 (hd day_list)    (* Real returning is hd day_list, which is an int*)
    else number_in_month (tl day_list, real_idx - 1) *)

fun number_in_month(day_list: (int*int*int) list, month: int) = 
    if null day_list
    then 0
    else 
        let 
            fun in_month(date: int*int*int) = (#2 date) = month
            fun cnt_in_month(is_in_month: bool) = 
                if is_in_month
                then 1
                else 0
        in 
            cnt_in_month(in_month(hd day_list)) + number_in_month(tl day_list, month)
        end

(* val test2 = number_in_month ([(2012,2,28),(2013,12,1)], 2) = 1
val test2_2 = number_in_month ([(2012,2,28),(2013,12,1)], 1) = 0
val test2_3 = number_in_month ([(2012,2,28),(2013,2,1)], 2) = 2 *)

fun number_in_months(day_list: (int*int*int) list, months: int list) = 
    if null months
    then 0
    else number_in_month(day_list, hd months) + number_in_months(day_list, tl months)

(* val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3 *)

fun dates_in_month(day_list: (int*int*int) list, month: int) = 
    if null day_list
    then []
    else 
        let 
            fun in_month(date: int*int*int) = (#2 date) = month
            val cons_list = dates_in_month(tl day_list, month)
            val list_hd = hd day_list
        in
            if in_month(list_hd)
            then (list_hd)::cons_list
            else cons_list
        end


(* val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)] *)


fun dates_in_months(day_list: (int*int*int) list, months: int list) = 
    if null months
    then []
    (* TODO: change this. *)
    else
        let 
            fun append_list(l1: (int*int*int) list, l2: (int*int*int) list) = 
                if null l1
                then l2
                else (hd l1)::append_list(tl l1, l2)
        in 
            append_list(dates_in_month(day_list, hd months), dates_in_months(day_list, tl months))
        end

(* val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)

fun get_nth(str_list: string list, nth: int) = 
    "nmsl"

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

fun date_to_string (year: int, month: int, day: int) = 
    "nmsl"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

fun number_before_reaching_sum(sum: int, ilist: int list) = 
    ~1

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

fun what_month(day: int) =
    ~1

val test9 = what_month 70 = 3

fun month_range (beg_day: int, end_day: int) = 
    [0]

val test10 = month_range (31, 34) = [1,2,2,2]

fun oldest(age_list: (int*int*int) list) = 
    NONE

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
