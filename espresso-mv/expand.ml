
open MultipleValued

(*
 * Return (adjacent_cubes, others)
 *)
let determine_adjacent_cubes (cube:cube_t) (set:sop_t) =
  List.partition (fun e -> cube_distance e cube = 1) set

let remove_essential_parts (set:sop_t) (off_set:sop_t) (overexpanded:cube_t) (free:cube_t) (f_raise:cube_t) =  
  let rec aux set off_set overexpanded free = match set with
    | cube::r -> 
      let (conflict_set, new_off_set) = determine_adjacent_cubes cube off_set in
      let new_free = List.fold_left (fun a e -> cube_diff a (cube_diff e cube)) free conflict_set in
      aux r new_off_set overexpanded new_free
    | [] -> (off_set, free, overexpanded)
  in aux set off_set overexpanded free

let remove_feasibly_covered (on_set:sop_t) (off_set:sop_t) (cube:cube_t) (free:cube_t) =
  let rec aux set accu cube free = match set with
    | f::r -> 
      let p = cube_supercube cube f in
      if not(List.exists (fun x -> cube_distance p x >= 1) off_set) then
        aux r accu p free(* TODO: Remove free part *)
      else aux r (f::accu) cube free
    | [] -> accu, cube 
  in aux on_set [] cube

let sop_expand (set:sop_t) (off_set:sop_t) =
  set
		      
		      
(*let sop_expand_single (on_set:sop_t) (off_set:sop_t) (cube:cube_t) =
  let to_raise = detect_feasibly_covered on_set off_set in
*)
  
