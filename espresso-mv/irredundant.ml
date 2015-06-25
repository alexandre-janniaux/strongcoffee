
open MultipleValued
open Partition
open Espresso

let build_irredundance_matrix (r_set:sop_t) =
  let n = List.length r_set in
  let matrix = Array.make_matrix n n false in
  List.iter (fun cube ->
    let h_c = sop_cofactor r_set cube in
    List.iter (fun r_pi -> ()
              ) r_set
  ) r_set


let sop_irredundant (accepts:sop_t) (dc:sop_t) : cube_t list =
  (* construire E_r et R_r *)
  let set = list_union accepts dc in
  let (ens_r_r,ens_e_r) =
    List.partition
      (fun cube -> 
         sop_contains (List.filter ((<>)cube) set) cube)
      accepts in
  let set_essential = list_union ens_e_r dc in
  let (ens_r_i, ens_r_t) = List.partition (sop_contains set_essential) ens_r_r in
  ens_r_t

