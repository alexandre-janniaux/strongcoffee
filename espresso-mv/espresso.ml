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
  debug "espresso" "Début de l'algorithme espresso sur (on/dc):";
  print_sop on_set;
  print_sop dc_set;

  let off_set = sop_complement union_set in
    (*if union_set <> [] then sop_complement union_set (* return Null cover ? *)
    else make_universe model*)
  debug "espresso" "calcul du complementaire : ";
  print_sop off_set;

  let total = list_union union_set off_set in
  if not (is_tautology total) then debug "espresso" "ATTENTION : on + off + dc != 1" else debug "espresso" "complémentaire OK";

  (*
   * Première expansion
   *)
  let set = sop_expand on_set off_set in
  debug "espresso" "expansion n°1";
  print_sop set;

  (*
   * Suppression des premières irredondances
   *)
  let set = sop_irredundant set dc_set in
  debug "espresso" "irredondance n°1";
  print_sop set;

  (*
   * On récupère les éléments essentiels
   *)
  let essentials = sop_essentials set dc_set in

  let cover = set in (*sop_difference expanded essentials in*)
  let forget = list_union dc_set essentials in

  let rec aux cover = 
    let c = sop_cost cover in
    let new_cover = sop_irredundant (sop_expand (sop_reduce cover on_set dc_set) off_set) dc_set in
    if sop_cost new_cover > c then aux new_cover
    else new_cover
  in 
  
  let new_cover = aux cover in
  new_cover

