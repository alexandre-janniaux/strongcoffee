open Format
open MultipleValued
open Partition
open Tautology
open MVDebug


let sop_merge_expensive (sop1:sop_t) (sop2:sop_t) =
  []

let simple_expand (sop:sop_t) = 
  let rec aux sop accu = match sop with
    | [] -> accu
    | x::r -> begin
        try 
          let y = List.find (fun y -> cube_distance x y = 1) r in 
          aux r ((cube_supercube x y)::accu)
        with Not_found -> aux r (x::accu)
      end
  in aux sop []
    
         

let sop_merge_fast (c1:cube_t) (sop1:sop_t) (c2:cube_t) (sop2:sop_t) =
  (*let merge set (accu,rest) cube = 
    if is_tautology (sop_cofactor set cube) then (* cube appartient à sop2 *)
      (cube::accu,rest) (* on a (c1+c2)d + (c2.sop2) = d  + c2.sop2 *)
    else
      (accu,cube::rest) (* que renvoyer si il n'y a pas de matching ? *)
  in
  let extracted, set = List.fold_left (merge sop1) [] sop2 in*)
  let set = (sop_intersect sop1 [c1]) @ (sop_intersect sop2 [c2]) (*|> sop_filter |> sop_filter_double*) in
  let rec aux set accu = match set with
    | x::r -> if List.exists (fun y -> cube_contains y x) (r@accu) then aux r accu else aux r (x::accu)
    | [] -> accu
  in aux set []

let one_var_dependance (cube:cube_t) : bool = 
  List.map (List.exists ((=)false)) cube 
  |> list_count ((=)true) 
     = 1 
      
let one_var_complement (cube:cube_t) : cube_t = 
  List.map (fun var -> 
    if List.exists ((=)false) var then
      literal_complement var
    else var
  ) cube

let extract_1_col (cube:cube_t) : cube_t = 
  let rec aux cube accu = 
    match cube with 
    | x::r -> 
      if List.exists ((=)false) x then 
        List.rev accu @ (x ::(List.map make_universe r))
      else aux r (x::accu)
    | _ -> List.rev accu
  in aux cube []

let rec sop_complement (sop:sop_t) =
  match sop with
  | [cube] -> 
      cube_complement cube
  | [] -> []
  | _ ->
    if sop_is_empty sop then [List.map make_universe (List.hd sop)]
    else if sop_is_universal sop then [List.map make_empty (List.hd sop)]
    else 
      let vect_or = extract_1_col (vect_or_col sop) in
      if List.exists (List.exists ((=)false)) vect_or then
        list_union (cube_complement vect_or) (sop_complement (sop_cofactor sop vect_or))
      else 
        let univars, other = List.partition one_var_dependance sop in
        if List.length univars <> 0 then 
          let f' = sop_complement other in
          List.fold_left (fun a x -> sop_intersect a [one_var_complement x]) f' univars
        else 
          let (p1, p2) = partition sop in
          let cof1, cof2 = sop_cofactor sop p1 |> simple_expand |> sop_filter |> sop_filter_double, 
                           sop_cofactor sop p2 |> simple_expand |> sop_filter |> sop_filter_double in

          let result = sop_merge_fast p1 (sop_complement cof1) p2 (sop_complement cof2) in
          result


			     

let sop_essentials (on_set:cube_t list) (dc_set:cube_t list) : cube_t list =
  let set = list_union on_set dc_set in
  let f l cube = 
    let sharp = set |> List.map (fun x -> cube_sharp x cube) |> List.flatten in
    let h = sop_consensus sharp [cube] in
    if not(sop_contains h cube || sop_contains dc_set cube) then cube::l
    else l
  in
  List.fold_left f [] on_set
  


		       
  
(*let espresso_mv on_set dc_set model = 
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
