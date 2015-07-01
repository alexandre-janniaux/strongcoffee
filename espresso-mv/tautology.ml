open Format
open MultipleValued
open Partition
open MVDebug

(* Check whether sop has a universal cube or not *)
let is_tautology_row (sop:sop_t) : bool =
  List.exists cube_is_universal sop


let vect_or_col (sop:sop_t) : cube_t = 
  list_map_n ( 
    list_map_n (
      List.exists ((=)true)
    )
  ) sop


(* Check whether sop has a variable never triggered or not *)
let is_not_tautology_col (sop:sop_t) : bool =
  not (
    sop |> 
    list_map_n (list_map_n (List.exists ((=)true))) |>
    List.map (List.exists ((=)true)) |>
    List.exists ((=)true)
  )

let vect_weakly_unate (sop:sop_t) : cube_t = 
  list_map_n (fun var_list ->
    if not (List.exists (List.exists ((=)false)) var_list) then List.hd var_list else begin
      let new_list = List.filter (List.exists ((=)false))  var_list in
      list_map_n (List.fold_left (fun a x -> a && not(x)) true) new_list
    end
  ) sop  

let is_weakly_unate (sop:sop_t) : bool = 
  not(List.exists (fun l -> not (List.exists ((=)true) l)) (vect_weakly_unate sop)) 

(* 
 * Check if sop is a tautology (i.e = 1 for any evaluation)
 * TODO: half-tail recursive
*)
let rec is_tautology (sop:sop_t) : bool = 
  if is_tautology_row sop then true
  else if is_not_tautology_col sop then false
  else if is_weakly_unate sop then false
  else
    let (c1,c2) = partition sop in
    let (f1,f2) = sop_cofactor sop c1 |> sop_filter, 
                  sop_cofactor sop c2 |> sop_filter in
    is_tautology f1 && is_tautology f2 


let find_tautology (sop:sop_t) : sop_t =
  let rec aux sop accu =
  if is_tautology_row sop then list_union sop accu
  else if is_not_tautology_col sop then accu
  else
    let (c1,c2) = partition sop in
    let (f1,f2) = sop_cofactor sop c1, sop_cofactor sop c2 in 
    aux f1 (aux f2 accu)
  in aux sop []


let sop_contains (sop:sop_t) (cube:cube_t) : bool =
  is_tautology (sop_cofactor sop cube) (* TODO: first linear cube_contains check ? *)

