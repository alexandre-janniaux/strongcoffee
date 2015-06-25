open MultipleValued
open Partition

(* Check whether sop has a universal cube or not *)
let is_tautology_row (sop:sop_t) : bool =
  List.exists cube_is_universal sop


(* Check whether sop has a variable never triggered or not *)
let is_not_tautology_col (sop:sop_t) : bool =
  not (
    sop |> 
    list_map_n (list_map_n (List.exists ((=)true))) |>
    List.map (List.exists ((=)true)) |>
    List.exists ((=)true)
  )


(* 
 * Check if sop is a tautology (i.e = 1 for any evaluation)
 * TODO: half-tail recursive
*)
let rec is_tautology (sop:sop_t) : bool = 
  if is_tautology_row sop then true
  else if is_not_tautology_col sop then false
  else
    let (c1,c2) = partition sop in
    let (f1,f2) = sop_cofactor sop c1, sop_cofactor sop c2 in
    is_tautology (sop_intersect f1 [c1]) && is_tautology (sop_intersect f2 [c2])
(* TODO CHECK : intersection nécessaire ? *)


