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
  let set = (sop_intersect sop1 [c1]) @ (sop_intersect sop2 [c2]) |> sop_filter |> sop_filter_double in
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


(*
 *    Prend un cube et remplie toutes ses variables
 *    sauf la première qui a un 0
 *
 *    Exemple : 1111 1010 10111 va être changé en 1111 1010 11111
 *)
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
    (*
     *    Si on nous passe une couverture toujours = 0,
     *    alors on a juste a renvoyer tout l'hypercube
     *)
    if sop_is_empty sop then [List.map make_universe (List.hd sop)]

    (*
     *    Au contraire, si on nous passe tout l'hypercube,
     *    alors on doit renvoyer un cube vide
     *)
    else if sop_is_universal sop then [List.map make_empty (List.hd sop)]

    (*
     *    Sinon, on est dans un cas raisonnable, et on va tenter
     *    de voir si l'on peut appliquer des cas de base, ou utiliser
     *    le paradigme recursive unate
     *)
    else 
      (*
       *     Optimisation (1) :
       *     Si la couverture a une colonne de 1, on peut l'extraire
       *     et faire le complément sur le reste 
       *)
      let vect_or = extract_1_col (vect_or_col sop) in
      if not(cube_is_universal vect_or) then 
        list_union (cube_complement vect_or) (sop_complement (sop_cofactor sop vect_or))

      else  
        (*
         *    Sinon, on cherche si les cubes de F ne dépendent que d'une seule variable
         *    et dans ce cas on applique le complément de De Morgan
         *    (X+Y+Z+other)' = X'*Y'*Z'*(other)'
         *)
        let univars, other = List.partition one_var_dependance sop in
        (*
         *    Si on peut bien trouver des cubes qui ne dépendent que d'une seule variable
         *)
        if List.length univars <> 0 then 
          (*
           *    On fait le complément du reste 
           *)
          let f' = sop_complement other in
          (*    
           *    Et on applique l'intersection avec les complémentaires des cubes univariées.
           *)
          List.fold_left (fun a x -> sop_intersect a [one_var_complement x]) f' univars

        else 
          (* 
           *    Sinon, on essaye un complément avec des fonctions weakly unate en utilisant
           *    "F=c * F_c" pour une fonction weakly unate en c. 
           *)
          let weak = vect_weakly_unate sop in 
          if List.exists (List.exists ((=)true)) weak then 
            let weak = List.map (List.map not) weak in
            let c = extract_1_col weak in 
            list_union (sop_complement (sop_cofactor sop c)) [one_var_complement c]

          else 
          (*
           *    Si on a pas de variables univariées, il ne nous reste que le calcul 
           *    en divisant le problème avec le cofacteur de Shannon
          *)
            let (p1, p2) = partition sop in
          (*
             *    TODO: Les simplification du cofacteur sont ils obligatoires ? 
            *)
            let cof1, cof2 = sop_cofactor sop p1 (*|> simple_expand |> sop_filter |> sop_filter_double |> sop_filter2*), 
                             sop_cofactor sop p2 (*|> simple_expand |> sop_filter |> sop_filter_double |> sop_filter2*) in
            begin
              let result = sop_merge_fast p1 (sop_complement cof1) p2 (sop_complement cof2) in
              result
            end




let sop_essentials (on_set:cube_t list) (dc_set:cube_t list) : cube_t list =
  let set = list_union on_set dc_set in
  let f l cube = 
    let sharp = set |> List.map (fun x -> cube_sharp x cube) |> List.flatten in
    let h = sop_consensus sharp [cube] in
    if not(sop_contains h cube || sop_contains dc_set cube) then cube::l
    else l
  in
  List.fold_left f [] on_set


