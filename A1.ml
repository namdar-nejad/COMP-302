let mode_tests: (int list * int) list = [
  ([1], 1);
  ([1;2;3;4;3;3], 3); 
  ([1;1;2;3;4;2;1;1;2;1;1;4;5;6], 1);
  ([21; 22; 32; 12; 21], 21);
  ([1;1;1;1;2;2;2;2], 1);
  ([1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;2],2);
  ([1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5],1);
  ([3;3;3;4;4;4;5;5;5;6;6;6;],3);
  ([3;3;3;4;4;4;5;5;5;6;6;6;6],6);
  ([100;100;100;1],100);
  ([1;100;100;1],1);
  ([1;0;0;1;100],0);
] ;;

let mode (l: 'a list) : 'a =
  let rec aux l ((cur_el, cur_num) : 'a * int) ((max_el, max_num) : 'a * int) = 
    match l with
    | [] -> max_el
    | next_el::t ->
        if cur_el = next_el then
          if cur_num+1 > max_num then aux t (next_el, cur_num+1) (cur_el, cur_num+1) 
          else aux t (next_el, cur_num+1) (max_el, max_num)
        else if cur_num > max_num then aux t (next_el, 1) (cur_el, cur_num)
        else aux t (next_el, 1) (max_el, max_num)
  in let h::t = List.sort compare l in
  aux t (h, 1) (h, 1)
;; 

(* Question 1.2 : Most common consecutive pairing *) 

let pair_mode_tests: (int list * (int * int) ) list = [
  ([1;1], (1,1));
  ([1;2;3;4;3;3], (1,2)); 
  ([1;1;2;3;4;2;1;1;2;1;1;4;5;6], (1,1));
  ([21; 21; 22; 22; 21], (21,21));
  ([1;1;1;2;2;2;], (1,1));
  ([1;1;1;2;2;2;2], (2,2));
  ([1;2;3;1;2;3], (1,2));
] ;;

let rec get_list (l: 'a list) (rtn : ('a *'a) list) : ('a * 'a) list = 
  match l with
  | [] -> rtn
  | [x] -> rtn
  | h::x::t -> get_list (x::t) ((h,x)::rtn)
;;


let pair_mode (l: 'a list) : 'a * 'a = 
  mode (get_list l [])
;; 


(* Section 2 : Custom data types *)


let convert_time ((from_unit, val_) : time_unit value) to_unit : time_unit value =
  match (from_unit, to_unit) with
  | Hour, Second -> (to_unit, (val_ *. 3600.))
  | Second, Hour -> (to_unit, (val_ /. 3600.))
  | _, _ -> (to_unit, val_)
;; 

let convert_dist ((from_unit, val_) : dist_unit value) to_unit : dist_unit value = 
  match (from_unit, to_unit) with
  | Foot, Meter -> (to_unit, (val_ *. 0.3048))
  | Meter, Foot -> (to_unit, (val_ /. 0.3048))
  | Meter, Mile -> (to_unit, (val_ /. 1609.3440))
  | Mile, Meter -> (to_unit, (val_ *. 1609.3440))
  | Foot, Mile -> (to_unit, (val_ /. 5280.)) 
  | Mile, Foot -> (to_unit, (val_ *. 5280.)) 
  | _,_ -> (to_unit, val_)
;;

let get_dist (distU_from, val_) distU_to =
  let (_,ans_val) =
    convert_dist (distU_from, val_)
      distU_to
  in
  ans_val
;;

let get_time (timeU_from, val_) timeU_to =
  let (_,ans_val) =
    convert_time (timeU_from, val_)
      timeU_to
  in
  ans_val
;;

let get_speed (distU_from, distU_to, timeU_from, timeU_to, val_)= 
  val_ *. (get_dist (distU_from, 1.) distU_to) /. (get_time (timeU_from, 1.) timeU_to)
;;

let convert_speed ((from_unit, val_) : speed_unit value) to_unit : speed_unit value = 
  let ((distU_from,timeU_from),(distU_to,timeU_to)) = (from_unit, to_unit)  in
  (to_unit, get_speed(distU_from, distU_to, timeU_from, timeU_to, val_))
;;

let add_speed (a : speed_unit value) ((b_unit, b_val) : speed_unit value) : speed_unit value = 
  let (((a_distU,a_timeU),a_val), (b_distU,b_timeU)) 
    = (a, b_unit)  in
  b_unit,(b_val +. get_speed(a_distU, b_distU, a_timeU, b_timeU, a_val))
;; 

let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  let ((b_distU,b_timeU),(a_timeU, a_value)) = (speed_unit,time) in
  (b_distU,(a_value *. speed_val *. get_speed(b_distU, b_distU, b_timeU, a_timeU, speed_val)))
;; 

let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  let ((b_distU,b_timeU),(a_timeU, a_value)) = (speed_unit,time) in
  (b_distU,(a_value *. get_speed(b_distU, b_distU, b_timeU, a_timeU, speed_val)))
;; 
 
(* Section 3 : recursive data types/induction *)

let passes_da_vinci_tests : (tree * bool) list = [
  ( Branch (5., [
        Branch (3., [Leaf; Leaf; Leaf]);
        Leaf;
        Branch (4., [])
      ]), true);
  ( Branch (3., [
        Branch (3., [Leaf; Leaf; Leaf]);
        Leaf;
        Branch (4., [])
      ]), false);
  ( Leaf, true); 
  ( Branch (5., [
        Branch (3., [Leaf; Leaf; Leaf]);
        Leaf;
        Branch (4., [])
      ]), true);
  ( Branch (0., [
        Branch (3., [Leaf; Leaf; Leaf]);
        Leaf;
        Branch (4., [Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Leaf])])
      ]), false);
  ( Branch (3., [
        Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Branch (3., [Leaf; Leaf; Leaf])]);
        Branch (3., [Leaf; Leaf; Leaf]);
        Branch (4., [])
      ]), false);
  ( Branch (6., [
        Branch (3., [Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Leaf]); Leaf; Leaf]);
        Branch (3., [Leaf; Leaf; Leaf]);
        Branch (4., [Leaf; Leaf; Leaf])
      ]), true);
  ( Branch (7., [
        Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Branch (3., [Leaf; Leaf; Leaf])]);
        Branch (3., [Leaf; Leaf; Leaf]);
        Branch (4., []);
        Branch (4., [])
      ]), false);
  ( Branch (7., [
        Branch (3., [Branch (3., [Leaf; Leaf; Leaf])]);
        Branch (3., [Leaf; Leaf; Leaf]);
        Branch (4., [Branch (3., [Leaf; Leaf; Leaf])]);
        Branch (3., [Leaf; Leaf; Leaf])
      ]), true);
  ( Branch (8., [
        Branch (3., [Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Leaf])]);
        Branch (3., [Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Leaf])]);
        Branch (4., []);
        Branch (4., [Leaf; Leaf; Leaf]);
      ]), true);
] ;;



let rec count l sum = 
  match l with
  | [] -> sum
  |h::t -> match h with
    | Leaf -> count t sum
    | Branch (f, list) -> count t sum+.(f*.f) 
;; 

let rec do_all f lst =
  match lst with 
  | x::xs -> if f x then do_all f xs
      else false
  | [] -> true
;;

let rec passes_da_vinci t =
  match t with 
  | Branch (f, list) -> 
      if ((f*.f) < count list 0.) then false 
      else do_all passes_da_vinci list
  | Leaf -> true 
;; 


(*
All inner helper functions (posted):

let mode_tests: (int list * int) list = [
  ([1], 1);
  ([1;2;3;4;3;3], 3); 
  ([1;1;2;3;4;2;1;1;2;1;1;4;5;6], 1);
  ([21; 22; 32; 12; 21], 21);
  ([1;1;1;1;2;2;2;2], 1);
  ([1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;2],2);
  ([1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5],1);
  ([3;3;3;4;4;4;5;5;5;6;6;6;],3);
  ([3;3;3;4;4;4;5;5;5;6;6;6;6],6);
  ([100;100;100;1],100);
  ([1;100;100;1],1);
  ([1;0;0;1;100],0);
] ;;

let mode (l: 'a list) : 'a =
  let rec aux l ((cur_el, cur_num) : 'a * int) ((max_el, max_num) : 'a * int) = 
    match l with
    | [] -> max_el
    | next_el::t ->
        if cur_el = next_el then
          if cur_num+1 > max_num then aux t (next_el, cur_num+1) (cur_el, cur_num+1) 
          else aux t (next_el, cur_num+1) (max_el, max_num)
        else if cur_num > max_num then aux t (next_el, 1) (cur_el, cur_num)
        else aux t (next_el, 1) (max_el, max_num)
  in let h::t = List.sort compare l in
  aux t (h, 1) (h, 1)
;; 

(* Question 1.2 : Most common consecutive pairing *) 

let pair_mode_tests: (int list * (int * int) ) list = [
  ([1;1], (1,1));
  ([1;2;3;4;3;3], (1,2)); 
  ([1;1;2;3;4;2;1;1;2;1;1;4;5;6], (1,1));
  ([21; 21; 22; 22; 21], (21,21));
  ([1;1;1;2;2;2;], (1,1));
  ([1;1;1;2;2;2;2], (2,2));
  ([1;2;3;1;2;3], (1,2));
] ;;


let pair_mode (l: 'a list) : 'a * 'a = 
  let rec get_list (l: 'a list) (rtn : ('a *'a) list) : ('a * 'a) list = 
    match l with
    | [] -> rtn
    | [x] -> rtn
    | h::x::t -> get_list (x::t) ((h,x)::rtn)
  in 
  mode (get_list l [])
;; 


(* Section 2 : Custom data types *)


let convert_time ((from_unit, val_) : time_unit value) to_unit : time_unit value =
  match (from_unit, to_unit) with
  | Hour, Second -> (to_unit, (val_ *. 3600.))
  | Second, Hour -> (to_unit, (val_ /. 3600.))
  | _, _ -> (to_unit, val_)
;; 

let convert_dist ((from_unit, val_) : dist_unit value) to_unit : dist_unit value = 
  match (from_unit, to_unit) with
  | Foot, Meter -> (to_unit, (val_ *. 0.3048))
  | Meter, Foot -> (to_unit, (val_ /. 0.3048))
  | Meter, Mile -> (to_unit, (val_ /. 1609.3440))
  | Mile, Meter -> (to_unit, (val_ *. 1609.3440))
  | Foot, Mile -> (to_unit, (val_ /. 5280.)) 
  | Mile, Foot -> (to_unit, (val_ *. 5280.)) 
  | _,_ -> (to_unit, val_)
;;


let convert_speed ((from_unit, val_) : speed_unit value) to_unit : speed_unit value = 
  let get_speed (distU_from, distU_to, timeU_from, timeU_to, val_)=
    let get_dist (distU_from, val_) distU_to =
      let (_,ans_val) =
        convert_dist (distU_from, val_)
          distU_to
      in
      ans_val in
    let get_time (timeU_from, val_) timeU_to =
      let (_,ans_val) =
        convert_time (timeU_from, val_)
          timeU_to
      in
      ans_val in
    val_ *. (get_dist (distU_from, 1.) distU_to) /. (get_time (timeU_from, 1.) timeU_to)
  in
  let ((distU_from,timeU_from),(distU_to,timeU_to)) = (from_unit, to_unit)  in
  (to_unit, get_speed(distU_from, distU_to, timeU_from, timeU_to, val_))
;;



let add_speed (a : speed_unit value) ((b_unit, b_val) : speed_unit value) : speed_unit value = 
  let get_speed (distU_from, distU_to, timeU_from, timeU_to, val_)=
    let get_dist (distU_from, val_) distU_to =
      let (_,ans_val) =
        convert_dist (distU_from, val_)
          distU_to
      in
      ans_val in
    let get_time (timeU_from, val_) timeU_to =
      let (_,ans_val) =
        convert_time (timeU_from, val_)
          timeU_to
      in
      ans_val in
    val_ *. (get_dist (distU_from, 1.) distU_to) /. (get_time (timeU_from, 1.) timeU_to)
  in
  let (((a_distU,a_timeU),a_val), (b_distU,b_timeU)) 
    = (a, b_unit)  in
  b_unit,(b_val +. get_speed(a_distU, b_distU, a_timeU, b_timeU, a_val))
;; 

let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  let get_speed (distU_from, distU_to, timeU_from, timeU_to, val_)=
    let get_dist (distU_from, val_) distU_to =
      let (_,ans_val) =
        convert_dist (distU_from, val_)
          distU_to
      in
      ans_val in
    let get_time (timeU_from, val_) timeU_to =
      let (_,ans_val) =
        convert_time (timeU_from, val_)
          timeU_to
      in
      ans_val in
    val_ *. (get_dist (distU_from, 1.) distU_to) /. (get_time (timeU_from, 1.) timeU_to)
  in
  let ((b_distU,b_timeU),(a_timeU, a_value)) = (speed_unit,time) in
  (b_distU,(a_value *. get_speed(b_distU, b_distU, b_timeU, a_timeU, speed_val)))
;; 
 
(* Section 3 : recursive data types/induction *)

let passes_da_vinci_tests : (tree * bool) list = [
  ( Branch (5., [
        Branch (3., [Leaf; Leaf; Leaf]);
        Leaf;
        Branch (4., [])
      ]), true);
  ( Branch (3., [
        Branch (3., [Leaf; Leaf; Leaf]);
        Leaf;
        Branch (4., [])
      ]), false);
  ( Leaf, true); 
  ( Branch (5., [
        Branch (3., [Leaf; Leaf; Leaf]);
        Leaf;
        Branch (4., [])
      ]), true);
  ( Branch (0., [
        Branch (3., [Leaf; Leaf; Leaf]);
        Leaf;
        Branch (4., [Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Leaf])])
      ]), false);
  ( Branch (3., [
        Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Branch (3., [Leaf; Leaf; Leaf])]);
        Branch (3., [Leaf; Leaf; Leaf]);
        Branch (4., [])
      ]), false);
  ( Branch (6., [
        Branch (3., [Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Leaf]); Leaf; Leaf]);
        Branch (3., [Leaf; Leaf; Leaf]);
        Branch (4., [Leaf; Leaf; Leaf])
      ]), true);
  ( Branch (7., [
        Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Branch (3., [Leaf; Leaf; Leaf])]);
        Branch (3., [Leaf; Leaf; Leaf]);
        Branch (4., []);
        Branch (4., [])
      ]), false);
  ( Branch (7., [
        Branch (3., [Branch (3., [Leaf; Leaf; Leaf])]);
        Branch (3., [Leaf; Leaf; Leaf]);
        Branch (4., [Branch (3., [Leaf; Leaf; Leaf])]);
        Branch (3., [Leaf; Leaf; Leaf])
      ]), true);
  ( Branch (8., [
        Branch (3., [Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Leaf])]);
        Branch (3., [Branch (3., [Branch (3., [Leaf; Leaf; Leaf]); Leaf; Leaf])]);
        Branch (4., []);
        Branch (4., [Leaf; Leaf; Leaf]);
      ]), true);
] ;;



let rec count l sum = 
  match l with
  | [] -> sum
  |h::t -> match h with
    | Leaf -> count t sum
    | Branch (f, list) -> count t sum+.(f*.f) 
;; 

let rec do_all f lst =
  match lst with 
  | x::xs -> if f x then do_all f xs
      else false
  | [] -> true
;;

let rec passes_da_vinci t =
  match t with 
  | Branch (f, list) -> 
      if ((f*.f) < count list 0.) then false 
      else do_all passes_da_vinci list
  | Leaf -> true 
;;

*)


  
  