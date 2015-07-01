open Format
open MultipleValued
open Partition
open Tautology
open Complement
open Expand
open Reduce
open Irredundant
open MVDebug


type espresso_config_t = { 
  nb_var : int;
  var_size : int list
}

let espresso_mv on_set dc_set espresso_config = 
  let union_set = list_union on_set dc_set in
  let off_set = sop_complement union_set
    (*if union_set <> [] then sop_complement union_set (* return Null cover ? *)
    else make_universe model*)
  in
  let expanded = sop_irredundant (sop_expand on_set off_set) dc_set in
  let essentials = sop_essentials expanded dc_set in
  let cover = expanded in (*sop_difference expanded essentials in*)
  let forget = list_union dc_set essentials in

  let rec aux cover = 
    let c = sop_cost cover in
    let new_cover = sop_irredundant (sop_expand (sop_reduce cover on_set dc_set) off_set) dc_set in
    if sop_cost new_cover > c then aux new_cover
    else new_cover
  in 
  
  let new_cover = aux cover in
  new_cover

