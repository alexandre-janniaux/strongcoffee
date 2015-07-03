
open MultipleValued
open Partition
open Complement
open Tautology
open Covering

(*
 *   Create a bool list list using the
 *   find_tautology tool. Rows and cols 
 *   are ordered according to the r_set
 *   order.
 *)
let build_irredundancy_matrix (r_set:sop_t) (ens_E:sop_t) =
  let ens_H = list_union ens_E r_set in

  let rec make_row cubes set accu = 
    match set with
    | cube::r -> 
      if List.mem cube cubes then
        make_row cubes r (true::accu)
      else
        make_row cubes r (false::accu)
    | [] -> List.rev accu
  in

  let rec aux set accu = match set with
    | cube::r ->
      let ens_H' = List.filter ((<>)cube) ens_H in
      let h_c = sop_cofactor ens_H' cube in
      let cubes = find_tautology h_c in 
      aux r ((make_row cubes r_set [])::accu)
    | [] -> List.rev accu
  in aux r_set []


(*  
 *  Remove irredundancy from the F=f^-1(\{1\}) set
 *  first by splitting the cover into essentials
 *  redundant and potentially redundant cover, then
 *  by building the covering matrix and solving the
 *  minimal cover problem.
 *)
let sop_irredundant (accepts:sop_t) (dc:sop_t) : cube_t list =
  (* construire E_r et R_r *)
  let set = list_union accepts dc in
  let (ens_R,ens_E) = List.partition (fun cube -> sop_contains (List.filter ((<>)cube) set) cube) accepts in
  let set_essential = list_union ens_E dc in
  let (ens_Ri, ens_Rt) = List.partition (sop_contains set_essential) ens_R in
  let matrix = build_irredundancy_matrix ens_Rt ens_E in
  let cover = solve_minimum_cover matrix ens_Rt  in
  list_union ens_E cover
