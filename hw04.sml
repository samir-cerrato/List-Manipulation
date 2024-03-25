
fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun illToString(l : int list list) : string =
    case l of
        [] => "[]"
      | x :: xs => "(" ^ ilToString x ^ ") :: " ^ illToString(xs)
            
fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")

fun testill (s : string) (n : (int list) list) (m : (int list) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ illToString m ^ "\n    Got: " ^ illToString n ^ "\n")

            
(* ---------------------------------------------------------------------- *)

(* Purpose: add n to each element of the list l
 * Examples:
 *  add_to_each ([], 7) ==> []
 *  add_to_each (1::2::3::[], 3) ==> 4::5::6::[]
 *  add_to_each (6::5::4::[], ~3) ==> 3::2::1::[]
 *)
fun add_to_each (l : int list, n : int) : int list =
    case l of
        [] => []
      | x::xs => x + n :: add_to_each (xs, n)

fun test_add_to_each() =
    (testil "ae1" (add_to_each ([], 7)) [];
     testil "ae2" (add_to_each ([1, 2, 3], 3)) [4, 5, 6];
     testil "ae2" (add_to_each ([6, 5, 4], ~3)) [3, 2, 1])

(* Purpose: computes the list of prefix sums for the argument list.  The
 *          i-th int in the result list is the sum of the first i int's
 *          in the argument list.
 * Examples:
 *  prefixSum [] ==> []
 *  prefixSum (1::2::3::[]) ==> 1::3::6::[]
 *  prefixSum (5::3::1::[]) ==> 5::8::9::[]
 *)
fun prefixSum (l : int list) : int list =
    case l of
      [] => []
    | x::xs => x :: add_to_each (prefixSum xs, x)

(* Tests for prefixSum *)
fun test_prefix_sum() =
    (testil "ps1" (prefixSum []) [];
     testil "ps2" (prefixSum [1,2,3]) [1,3,6];
     testil "ps3" (prefixSum [5,3,1]) [5,8,9])


(* TASK *)
(*Purpose: Uses an additional arguement to compute the prefix sum operation in 
time that is asympototically better. Sums int n to the first element in the list and
then sums to the next element in the list. 
Example:
prefixSumHelp ([] , 7) = []
prefixSumHelp ([1,2,3] , 2) = [3,5,8]
prefixSumHelp ([2,3,4] , 1) = [3,6,10]
prefixSumHelp ([3,4,5] , 3) = [6,10,15]*)
fun prefixSumHelp (l : int list, n : int) : int list =
    case l of
        [] => []
        | x :: xs => x + n :: prefixSumHelp(xs, x+n)

fun test_pshelp() =
    (testil "psh1" (prefixSumHelp ([], 7)) [] ; 
    testil "psh2" (prefixSumHelp ([1,2,3], 2)) [3,5,8] ; 
    testil "psh3" (prefixSumHelp ([2,3,4], 1)) [3,6,10] ;
    testil "psh4" (prefixSumHelp ([3,4,5], 3)) [6,10,15]
     )

(* TASK *)
(*Puropse: sums the first element in the list to the next element and it continues
through the entire list.
Example:
prefixSumFast [1,2,3] = [1,3,6]
prefixSumFast [2,4,6] = [2,6,12]
prefixSumFast [3,5,1] = [3,8,9]
prefixSumFast [6,2,2] = [6,8,10]*)
fun prefixSumFast (l : int list) : int list =
    prefixSumHelp(l , 0)

fun test_psfast() =    
    (testil "psf2" (prefixSumFast [1,2,3]) [1,3,6] ;
     testil "psf3" (prefixSumFast [2,4,6]) [2,6,12] ;
     testil "psf4" (prefixSumFast [3,5,1]) [3,8,9] ;
     testil "psf5" (prefixSumFast [6,2,2]) [6,8,10]
    )
        
(* ---------------------------------------------------------------------- *)

(* TASK *)
(*Purpose: makes a list of the elements of l less than the pivot*)
fun filter_less (l : int list, bound : int) : int list =
    case l of
        [] => []
        | x :: xs => case (x < bound) of
                     true => x :: (filter_less(xs,bound))
                     | false => filter_less(xs,bound)

(*Purpose: makes a list of the elements of l greater than or equal to the pivot*)
fun filter_greatereq (l : int list, bound : int) : int list =
    case l of   
        [] => []
        | x :: xs => case (x >= bound) of
                        true => x :: (filter_greatereq(xs,bound))
                        | false => filter_greatereq(xs,bound)

fun test_filter() =
    (testil "f1" (filter_less([42,5,13,83,1,1,34,3,5,2],4)) [1,1,3,2];
     testil "f2" (filter_greatereq([1,2,3,4,5],2)) [2,3,4,5];
     testil "f3" (filter_less([42,5,13,83,1,1,34,3,5,2], 45)) [42, 5, 13, 1, 1, 34, 3, 5, 2];
     testil "f4" (filter_greatereq([1,2,3,4,5],3)) [3,4,5];
     testil "f5" (filter_less([1,3,5,7,9,11], 7)) [1,3,5];
     testil "f6" (filter_greatereq([2,45,24,41,56,50], 25)) [45,41,56,50]
     )
        
(* TASK *)
(*Purpose: sorts the elements of l in increasing order
Example:
quicksort_l [9,8,7,6,5,4,3] = [3,4,5,6,7,8,9]
quicksort_l [2,1,4,3,7,5,6] = [1,2,3,4,5,6,7]
quicksort_l [100,39,90,4,7,101] = [4,7,39,90,100,101]
quicksort_l [5,20,15,10,35,30] = [5,10,15,20,30,35]*)
fun quicksort_l (l : int list) : int list =
    case l of 
        [] => []
        | x :: xs => let val g = filter_greatereq(xs,x)
                        val l = filter_less(xs,x)
                        val gs = quicksort_l(g)
                        val ls = quicksort_l(l)
                    in ls @ [x] @ gs end
        
fun test_qs() =
     (testil "qs1" (quicksort_l [9,8,7,6,5,4,3]) [3,4,5,6,7,8,9];
      testil "qs2" (quicksort_l [2,1,4,3,7,5,6]) [1,2,3,4,5,6,7];
      testil "qs3" (quicksort_l [100,39,90,4,7,101]) [4,7,39,90,100,101];
      testil "qs4" (quicksort_l [5,20,15,10,35,30]) [5,10,15,20,30,35]
      )

(* ---------------------------------------------------------------------- *)

fun run() =
    (test_add_to_each();
     test_prefix_sum();
     test_pshelp();
     test_psfast();
     test_filter();
     test_qs()
     )

