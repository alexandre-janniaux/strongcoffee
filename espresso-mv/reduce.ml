open MultipleValued
open Complement 
open Partition

(* Goal : Compute supercube (c#(FUD)-c) efficiently*) 

(* (1) Order the cube using MINI heuristic algorithm 
 * (2) 
 *
 *)

let sop_or_column (f:sop_t) = 
  list_map_n (fun vars -> (* pour chaque variable *) 
    list_map_n (fun x ->  (* pour chaque valeur que peut prendre la variable *)
      List.exists ((=)true) x (* vaut vrai si on a une colonne de 0 *)
    ) vars
  ) f

let sop_and_column (f:sop_t) = 
  list_map_n (fun vars ->
    list_map_n (fun x -> 
      not (List.exists ((=)false) x)
    ) vars
  ) f 

let rec sop_supercube_complement (sop:sop_t) : cube_t = 
  match sop with 
  | [] -> []
  | [c] -> sop_supercube (cube_complement c)
    (*let nb_var = list_count (List.exists ((=)false)) li*)
  | _ -> 
    let c = sop_or_column sop in
    if not (cube_is_universal c) then 
      let c' = cube_complement c in 
      let super_c = sop_supercube c' in
      cube_supercube (sop_supercube_complement (sop_cofactor sop c)) super_c
    else 
      let c,c' = partition sop in
      let extract c = cube_intersect (sop_supercube_complement (sop_cofactor sop c)) c in  
      let f1 = extract c and f2 = extract c' in
      cube_supercube f1 f2 

let sop_reduce (sop:sop_t) (on_set:sop_t) (dc_set:sop_t) : sop_t = 
  let set = list_union on_set dc_set in
  let rec aux sop set accu = 
    match sop with 
    | cube::r -> 
      let set' = List.filter ((<>)cube) set in
      let c' = cube_intersect cube (sop_supercube_complement set') in
      aux r (c'::set') (c'::accu)
    | _ -> accu
  in aux sop set []
