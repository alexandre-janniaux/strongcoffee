(*#use "cover_lib.ml";;*)

open MultipleValued
open Partition
open Tautology


let sop_contains (sop:sop_t) (cube:cube_t) : bool =
  is_tautology (sop_cofactor sop cube) (* TODO: first linear cube_contains check ? *)

    
let sop_merge_expensive (sop1:sop_t) (sop2:sop_t) =
  []

    
let sop_merge_fast (c1:cube_t) (sop1:sop_t) (c2:cube_t) (sop2:sop_t) =
  List.fold_left
    (fun accu cube ->
      if is_tautology (sop_cofactor sop2 cube) then (* cube appartient à sop2 *)
	cube::accu (* on a (c1+c2)d + (c2.sop2) = d  + c2.sop2 *)
      else
	accu (* que renvoyer si il n'y a pas de matching ? *)
    )
    [] sop1


let rec sop_complement (sop:sop_t) =
  match sop with
  | [cube] -> cube_complement cube
  | [] -> []
  | _ ->
    let (p1, p2) = partition sop in
    let cof1, cof2 = sop_cofactor sop p1, sop_cofactor sop p2 in
    sop_merge_fast p1 (sop_complement cof1) p2 (sop_complement cof2)

  
  
			     

let sop_essentials (on_set:cube_t list) (dc_set:cube_t list) : cube_t list =
  let set = list_union on_set dc_set in
  let f l cube = 
    let sharp = set |> List.map (fun x -> cube_sharp x cube) |> List.flatten in
    let h = sop_consensus sharp [cube] in
    if not(sop_contains h cube || sop_contains dc_set cube) then cube::l
    else l
  in
  List.fold_left f [] on_set
  


		       
  
  
	
  
  

(*
let sop_union sop1 sop2 = 
;;

*)

(*
let sop_expand (sop:sop_t) (off_set:cube_t list) =

;;
*)
(*
let sop_reduce (sop:sop_t) (dc_set:cube_t list) =
;;
*)
(*


let minterm_intersect min1 min2 = 
  let rec aux 

let cover_union cov1 cov2 = 
  let rec aux cov accu = 
    match cov with 
      | x::r -> 
          if List.mem x accu
            aux r (x::accu)
          else aux r accu
      | [] -> accu
  in aux cov1 cov2
;;

let minterm_supercube min1 min2 = 

let cover_expand  = 
  false ;;


let cover_irredundant cover dc_set = false
;;


 
let espresso_mv on_set dc_set model = 
  let union_set = list_union on_set dc_set in
  let off_set = 
    if union_set <> [] then sop_complement union_set (* return Null cover ? *)
    else make_universe model
  in
  let expanded = sop_irredundant (cover_expand on_set off_set) dc_set in
  let essentials = cover_find_essentials (expanded dc_set) in
  let cover = cover_difference expanded essentials in
  let forget = cover_union dc_set essentials in

  let rec aux cover = 
    let c = cover_cost cover in
    let new_cover = irredundant (expand (reduce cover dc_set) off_set) dc_cover in
    if cost new_cover > c then aux new_cover
    else new_cover
  in 
  
  let new_cover = aux cover in

*)
