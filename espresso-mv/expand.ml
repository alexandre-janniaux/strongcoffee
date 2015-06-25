
open MultipleValued

(*
let detetermine_essential_parts (off_set:sop_t) (cube:cube_t) (expansion:cube_t) =
  let rec aux off_set cube expansion =
    try let e = List.find (fun e -> cube_distance e cube = 1) off_set in
    with Not_found -> (off_set, cube, free)
  in aux cube free

let detect_feasibly_covered (on_set:sop_t) (off_set:sop_t) (cube:cube_t) =
  try Some (
	  List.find (fun f ->
		     let p = cube_supercube f cube in
		     not @@ List.exists (fun x -> cube_distance p x >= 1) off_set
		    ) on_set)
  with Not_found -> None 
		      
		      
let sop_expand_single (on_set:sop_t) (off_set:sop_t) (cube:cube_t) =
  let to_raise = detect_feasibly_covered on_set off_set in
  *)
