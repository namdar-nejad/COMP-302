
(* Question 1 *)

(* Tail Reccursive *)
let rec find_map (f : 'a -> 'b option) (l : 'a list) : 'b option =
  match l with 
  | [] -> None
  | h::t ->
      if (f h <> None) then f h
      else find_map f t
;; 

let partition (p : 'a -> bool) (l : 'a list) : ('a list * 'a list) =
  let split a (l1, l2) =
    if (p a) then (a::l1,l2)
    else (l1,a::l2)
  in
  List.fold_right split l ([],[])
;;

(* Question 2 *)

let make_manager (masterpass : masterpass) : pass_manager = 
  let ref_list : (address * password) list ref = ref [] in 
  let main_pass = ref masterpass in
  let num_wrong = ref 0 in
  let num_right = ref 0 in
  let locked = ref false in 
  let increment_right () = (num_right := (!num_right+1)) in
  
  let pass_match mpass =
    if (mpass = !main_pass) then (num_wrong := 0; true)
    else (num_wrong := (!num_wrong+1);
          if (!num_wrong >= 3) then
            (locked := true; raise WrongPassword;)
          else raise WrongPassword;) in
  
  let save =
    (fun mpass addr pass ->
       if (not !locked) then 
         (if (pass_match mpass)
          then (increment_right ();
                ref_list := (addr, encrypt mpass pass)::!ref_list;))
       else raise AccountLocked) in

  let get_force = (fun mpass addr ->
      
      let func_address_map (x : (address * password)) : password option =
        let (addressx, passx) = x in
        if addressx = addr then Some passx
        else None in
      
      let res = find_map func_address_map !ref_list in 
      match res with
      | None -> None
      | Some x -> Some (decrypt mpass x)) in 

  let get = (fun mpass address ->
      if (not !locked) then 
        (if (pass_match mpass)
         then increment_right (); (get_force mpass address))
      else raise AccountLocked ) in

  let update_master = (fun mpass_cur mpass_new -> 
      if (pass_match mpass_cur)
      then (main_pass := mpass_new); increment_right (); locked := false;
      ref_list := List.map (fun (addr_cur, old_pass) ->
          (addr_cur, (encrypt mpass_new (decrypt mpass_cur old_pass)))) !ref_list) in
  
      (* let old_list = !ref_list in
        let ref_list = ref [] in
           
        let rec do_all old_l new_l =
          match old_l with
          | [] -> ()
          | h::t -> let (addr, old_pass) = h in
              ((save mpass_new addr (decrypt mpass_cur old_pass));
               do_all t new_l)
               
        in do_all old_list ref_list) in *)

  let count_ops =
    (fun mpass -> 
       if (not !locked) then 
         (if (pass_match mpass) then increment_right ();!num_right)
       else raise AccountLocked) in
  
  {save; get_force; get; update_master; count_ops} 
;;



(* Question 3 *)

(* Counting values at same time *)
let catalan_count (n : int) : (int * int) = 
  let count_rec_calls = ref 0 in 
  let rec catalan_inner n' =
    count_rec_calls:= !count_rec_calls + 1;
    if n' = 0 then 1
    else 
      let rec aux i n' acc = 
        if i > n' then acc
        else
          aux (i + 1) n' (acc + catalan_inner i * catalan_inner (n' - i))
      in
      aux 0 (n'-1) 0
  in
  let rtn = catalan_inner n
  in (rtn, !count_rec_calls)
;; 

(* Memoization function *)
let memoize (f : ('a -> 'b) -> 'a -> 'b) (stats : stats): 'a -> 'b =
  let hash = Hashtbl.create 1000 in 
  let rec f' x = 
    match Hashtbl.find_opt hash x with
    | None ->
        let y = f f' x in
        Hashtbl.add hash x y;
        stats.entries := !(stats.entries)+1; y 
    | Some y -> stats.lkp := !(stats.lkp)+1; y
  in f'
;;

(* Version of catalan that can be memoized *)
let memo_cat (recf : int -> int) (n : int) : int = 
  if n = 0 then 1
  else 
    let rec aux i n acc = 
      if i > n then acc
      else aux (i + 1) n (acc + recf i * recf (n - i))
    in
    aux 0 (n-1) 0
;;

let catalan_m (n : int) : int * stats = 
  let st = {entries = ref 0; lkp = ref 0} in 
  ((memoize memo_cat) st n, st)
;;

