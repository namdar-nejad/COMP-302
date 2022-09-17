(* Reminder: Do not modify the function signature. Make a backup of your code regularly. Do not define external helper functions, unless the exercise explictly tells you to do so. *)

(* 
    Section 1 : Fix Me 

    First, correct and add test cases by adding elements to `distance_tests`, `hanoi_tests` and `fact_tests`.
Then, correct the implementation of functions `distance`, `hanoi` and `fact`.
                                                                             *)


(* Question 1.1 : Lattice 2D Euclidean distance *)

let distance_tests = [
  ( ((0, 0), (3, 4)), (5.) );
  ( ((0, 0), (1, 1)), (1.414214) );
  ( ((0, 0), (0, 0)), (0.) );
  ( ((1, 1), (1, 1)), (0.) );
  ( ((1, 0), (0, 1)), (1.414214) );
  ( ((0, 5), (5, 1)), (6.403124) );
  ( ((10, 1), (1, 0)), (9.055385) );
  ( ((0, 3), (10, 5)), (10.198039) );
  ( ((9, 3), (9, 5)), (2.) );
  ( ((0,1), (0,2)), (1.) );
  ( ((0,0), (-1,-1)), (1.414214) );
  ( ((-2,-2), (2,5)), (8.062258) );
  ( ((-2,0), (0,1)), (2.236068) );
  ( ((-2,4), (4,-2)), (8.485281) );
  ( ((-2,-3), (4,-2)), (6.082763) );
  ( ((-2,-30), (0,-10)), (20.099751) );
  ( ((0,0), (2,-1)), (2.236068) );
  ( ((-1,-1), (-3,5)), (6.324555) );
  ( ((-1,0), (-5,-1)), (4.123106) );
]
;;

let distance ((x1, y1): (int * int)) ((x2, y2): (int * int)) : float =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let tmp = float_of_int (dx * dx + dy * dy) in
  sqrt (tmp)
;;


(* Question 1.2 : Tower of Hanoi recursive definition *)

let hanoi_tests = [
  (1, 1);
  (2, 3);
  (3, 7);
  (4, 15);
  (5, 31);
  (6, 63);
  (7, 127);
  (8, 255);
  (9, 511);
  (10, 1023);
  (11, 2047);
  (12, 4095);
]
;;

let rec hanoi (n: int) : int =
  if n < 1 then domain ()
  else if n = 1 then 1
  else 2* hanoi (n-1) + 1
;;


(* Question 1.3 : non-tail-recursive factorial *)

let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (3, 6.);
  (4, 24.);
  (5, 120.);
  (8, 40320.);
  (10, 3628800.);
] 
;;


let rec fact (n: int): float = 
  if n < 0 then domain () else
  if n = 0 then 1. else
    fact (n - 1) *. float_of_int(n)
;; 

(* 
    Section 2: Hailstone sequence 
    Add tests to `collatz_tests` and implement both `collatz_helper` and `collatz`.
*)

let collatz_tests = [
  (1, 0);
  (2, 1);
  (3, 7);
  (4, 2);
  (5, 5);
  (6, 8);
  (7, 16);
  (8, 3);
  (9, 19);
  (10, 6);
  (11, 14);
  (12, 9);
  (13, 9);
  (14, 17);
  (15, 17);
  (16, 4);
  (17, 12);
  (18, 20);
  (19, 20);
  (20, 7);
  (50, 24);
  (51, 24);
  (52, 11);
  (53, 11);
  (54, 112);
  (55, 112);
  (56, 19);
  (57, 32);
  (58, 19);
  (59, 32);
] 
;;

let rec collatz_helper (n: int) (steps_so_far: int) : int = 
  if n = 1 then steps_so_far
  else if (n mod 2 = 0) then collatz_helper (n / 2) (steps_so_far+1)
  else collatz_helper (3*n+1) (steps_so_far+1)
;;

let collatz (n: int) : int = 
  if n < 1 then domain ()
  else collatz_helper n 0
;;


(* 
    Section 3: Riemann Zeta function 
    Implement the `approx_zeta` function.
    You do not need to modify any other parts inside the `zeta` function
*)

(* Your job is to implement a function approx_zeta that approximates the value ζ(k)
for a given float k, as well as an accuracy value acc. As this is about adding more
and more terms, your function's job is to stop once the next term to add is below acc.
                                                                                    As usual, first write up some tests in the list named zeta_tests. One way you can get
  the expected outputs for any inputs for your tests is via WolframAlpha; search for
"zeta function at s = 3" for example.

                               We use epsilon_float as the desired accuracy, which in OCaml is the difference between 1.0
and the smallest exactly representable floating-point number greater than 1.0.

                                                                            Note that we made approx_zeta a local function to be defined inside the function zeta k.
                                                                                                                                                                    This function also makes sure that your function receives values of k larger than 2 -
                                                                                                                                                                                                                                        while this function is well-defined for k even for numbers between 1 and 2, the evaluation
                                                                                                                                                                                                                                                                                                                   would take too long for such arguments.
*)

let zeta_tests = [
  (3., 1.2020569031);
  (5., 1.0369277551);
  (6., 1.0173430619);
  (7., 1.0083492773);
  (8., 1.0040773561);
  (10., 1.0009945751);
] 
;;

let zeta (k: float) : float = 
  let rec approx_zeta k acc n sum_so_far = 
    let term = n ** (-.k) 
    in 
    if term < acc then sum_so_far
    else let sum_so_far = sum_so_far +. term
      in 
      approx_zeta k acc (n +. 1.) sum_so_far
  in
  if k < 2. then domain () 
  else approx_zeta k epsilon_float 1. 0.
;;

