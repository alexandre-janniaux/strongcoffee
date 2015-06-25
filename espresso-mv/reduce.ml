open MultipleValued
open Espresso


(* Goal : Compute supercube (c#(FUD)-c) efficiently*) 

(* (1) Order the cube using MINI heuristic algorithm 
 * (2) 
 *
 *)

let find_zero_column (f:sop_t) = 
  list_map_n (fun vars -> (* pour chaque variable *) 
    list_map_n (fun x ->  (* pour chaque valeur que peut prendre la variable *)
      not (List.exists ((=)true) x) (* vaut vrai si on a une colonne de 0 *)
    
(*
let rec sop_supercube_complement (f:sop_t) = 
  match f with 
  | [c] -> cube_supercube c
  | _::_ ->
  | [] -> 
*)

(*let reduce =*)
