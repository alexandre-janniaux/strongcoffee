
open MultipleValued
open Partition
open Espresso
open Tautology

let build_irredundancy_matrix (r_set:sop_t) (ignore_set:sop_t) =
  let n = List.length r_set in
  let matrix = Array.make_matrix n n false in
  list_enumerate (fun i cube ->
    let h_c = sop_cofactor r_set cube in
    let cubes = find_tautology h_c in 
    list_enumerate (fun j x -> 
      if List.mem x cubes then
        matrix.(i).(j) <- true
    ) r_set
  ) r_set;
  matrix


let sop_irredundant (accepts:sop_t) (dc:sop_t) : cube_t list =
  (* construire E_r et R_r *)
  let set = list_union accepts dc in
  let (ens_r_r,ens_e_r) = List.partition (fun cube -> sop_contains (List.filter ((<>)cube) set) cube) accepts in
  let set_essential = list_union ens_e_r dc in
  let (ens_r_i, ens_r_t) = List.partition (sop_contains set_essential) ens_r_r in
  (*let matrix = build_irredundancy_matrix ens_r_t in
  let cover = solve_minimum_cover matrix in
  list_union ens_e_r cover*)
  ens_e_r
