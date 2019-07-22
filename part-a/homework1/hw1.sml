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

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

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


val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
