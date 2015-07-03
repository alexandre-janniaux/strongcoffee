open Format
open MVDebug
open MultipleValued
open Complement 
open Partition
open Tautology

(* Goal : Compute supercube (c#(FUD)-c) efficiently*) 

(* (1) Order the cube using MINI heuristic algorithm 
 * (2) 
 *
 *)

let sop_or_column (f:sop_t) = 
  list_map_n (fun vars -> (* pour chaque variable *) 
    list_map_n (fun x ->  (* pour chaque valeur que peut prendre la variable *)
      List.exists ((=)true) x (* vaut vrai si on a une colonne de 0 *)
    ) vars
  ) f

let sop_and_column (f:sop_t) = 
  list_map_n (fun vars ->
    list_map_n (fun x -> 
      not (List.exists ((=)false) x)
    ) vars
  ) f 

(*
 *    Compute the supercube of the complement of a cover
 *    in an efficient way.
 *)
let rec sop_supercube_complement (sop:sop_t) : cube_t = 
  match sop with 
  | [] -> []
  | [c] ->
    (* 
     *  S'il n'y a qu'un seul cube, le complément est facile à calculer
     *  et le supercbe se calule aisément à partir de son résultat.
     *)
    sop_supercube (cube_complement c)
    (*let nb_var = list_count (List.exists ((=)false)) li*)
  | _ -> 
    if sop_is_universal sop then 
      List.map make_empty (List.hd sop)
    else begin
    (*
     *  Sinon, cherchons les cas de base ou divisions l'opération en
     *  des opérations plus simple
     *)

    (*
     *  Première optimisation : si la couverture a une colonne de 0
     *  alors on forme "c" le cube qui a un 0 sur la colonne désignée
     *  et on a l'identité F = c * F_c. De plus, c' est un unique cube
     *  donc supercube(F') = supercube ( supercube (F_c'), supercube (c') )
     *)
    let c = sop_or_column sop in

    (* Si on est dans le cas d'une colonne de 0 *)
    if not (cube_is_universal c) then begin
      (*debug "reduce" "Colonne de 0 trouvée.";*)
      (*
       *  On a déjà le cube voulu, on applique alors le résultat 
       *)
      let c_compl = cube_complement c in
      let super_c = sop_supercube c_compl in 
      if cube_is_universal super_c then 
        super_c
      else if cube_is_empty super_c then
        sop_supercube_complement (sop_cofactor sop c)
      else begin
          cube_supercube (sop_supercube_complement (sop_cofactor sop c)) super_c 
      end

      
    end else if is_weakly_unate sop then begin
      (*
       *  Si la couverture est weakly unate, on peut calculer le supercube
       *  de façon très efficace comme l'intersection des supercubes des compléments
       *  de chaque cube
      *)
      let first = sop_supercube (cube_complement (List.hd sop)) in
      List.fold_left (fun a x -> cube_intersect a (sop_supercube (cube_complement x))) first (List.tl sop)

    end else begin
      (*
       *  Sinon, on va séparer le cube en prenant une partition c,c'
       *  et on applique "supercube(F') = supercube( c*supercube(F'_c) + c'*supercube(F'_c') )"
      *)
      let c,c' = partition sop in

      let extract c = 
        let cof = sop_cofactor sop c in
        cube_intersect (sop_supercube_complement cof) c 
      in  

      let f1 = extract c and f2 = extract c' in 
        cube_supercube f1 f2 
    end
  end

let sop_reduce (sop:sop_t) (on_set:sop_t) (dc_set:sop_t) : sop_t = 
  let set = list_union on_set dc_set in
  let rec aux sop set accu = 
    match sop with 
    | cube::r -> 
      let set' = List.filter ((<>)cube) set in (* TODO: améliorer l'extraction *)
      let c' = cube_intersect cube (sop_supercube_complement set') in
      aux r (c'::set') (c'::accu)
    | _ -> accu
  in aux sop set []
