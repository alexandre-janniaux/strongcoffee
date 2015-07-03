open Format
open MultipleValued
open Covering

let cube_lower cube lower = 
  List.map2 
    (List.map2 (fun x y -> x && (not y)))
    cube lower 

let cube_upper cube upper =
  List.map2
    (List.map2 (fun x y -> x || not(y)))
    cube upper


(*
 *    Step (1) of the expand algorithm
 *    Return (adjacent_cubes, others)
 *)
let determine_adjacent_cubes cube set =
  List.partition (fun e -> cube_distance e cube = 1) set

let remove_essential_parts cube off_set free =  
  let (conflict_set, new_off_set) = determine_adjacent_cubes cube off_set in
  let free_cube = List.fold_left (fun a e -> cube_lower a (cube_diff e cube)) free conflict_set in
  let raise_cube = List.fold_left (fun a e -> cube_upper a e) cube conflict_set in
  let overexpanded = cube_supercube free_cube raise_cube in
  let final_off_set = List.filter (fun x -> cube_distance x overexpanded = 0) new_off_set in
  final_off_set, free_cube, raise_cube




let remove_feasibly_covered on_set off_set cube raise_cube free =
  let rec aux cube free_cube raise_cube on_set =
    let covered_cubes, others = 
      List.partition (fun f -> 
        let p = cube_supercube cube f in 
        not(List.exists (fun x -> cube_distance p x >= 1) off_set) && p <> cube)
        on_set 
    in

    let func x = let supercube = cube_supercube cube x in 
      list_count ((=)true) (List.map (fun y -> cube_contains supercube y) covered_cubes)
    in

    let map_score = List.map func on_set in

    if map_score = [] then
      off_set, free_cube, raise_cube
    
    else begin
      let best_id = select_score (>) map_score in
      let best, set' = list_pop on_set best_id in

      let free_cube = cube_lower free_cube best in
      let raise_cube = cube_upper raise_cube best in
      let new_cube = cube_supercube best cube in

      if free_cube |>  List.filter (literal_is_empty) = [] then
        off_set, free_cube, raise_cube
      else
        aux new_cube free_cube raise_cube set'
    end
  in aux cube free raise_cube on_set



let expand_most_frequent free_cube set =
  free_cube



let expand_minimum_covering off_set raise_cube free_cube =   
  raise_cube



let expand_minimum_covering_random off_set raise_cube free_cube = 
  raise_cube
  
  



let cube_expand cube on_set off_set = 
  let free_cube = List.map (List.map not) cube in

  (* STEP 1 : determination of essential parts *)
  let off_set, free_cube, raise_cube = 
    remove_essential_parts cube off_set free_cube in
  raise_cube

  (*(* STEP 2 : Detection of feasibly covered cubes *)
  let off_set, free_cube, raise_cube = 
    remove_feasibly_covered on_set off_set cube raise_cube free_cube in

  (* STEP 3 : Finding the largest prime implicant covering the cube *)
  let free_cube = expand_most_frequent free_cube on_set in

  (* STEP 4 : Expansion via the minimum covering problem *)
  raise_cube *)
  

let sop_expand set off_set =
  let rec aux set accu = 
    match set with
    | [] -> accu
    | cube::r -> aux r ((cube_expand cube r off_set)::accu)
  in aux set []
		      
		      
(*let sop_expand_single (on_set:sop_t) (off_set:sop_t) (cube:cube_t) =
  let to_raise = detect_feasibly_covered on_set off_set in
*)
  
