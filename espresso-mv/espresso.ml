(*#use "cover_lib.ml";;*)



type literal_t = bool list
type cube_t = literal_t list
type sop_t = cube_t list
  
type primitive = 
| Literal of literal_t
| Cube of cube_t
| Sop of sop_t

(* douple pipe operator *)
let (|>>) (a,b) f =
  f a b
;;

let range n =
  let rec aux k accu =
    if k < 0 then accu
    else aux (k-1) (k::accu)
  in aux (n-1) []

  
let list_union l1 l2 =
  let f l x = if List.mem x l then l else x::l in 
  List.fold_left f l2 l1


let list_count f l =
  List.fold_left (fun a x -> if f x then a+1 else a) 0 l


let list_sum l =
  List.fold_left (fun a x -> x+a) 0 l


let list_map_n (f:'a list -> 'b) (l:'a list list) : 'b list =
  let rec depile l = (List.map List.hd l, List.map List.tl l |> List.filter ((<>)[])) in
  let rec aux l accu =
    match l with
    | [] -> List.rev accu
    | _ ->
      let (t,r) = depile l in
      aux r ((f t)::accu)
  in aux l []


let literal_is_empty (v:literal_t) : bool =
  not (List.exists ((=)true) v)

(*
 * S'il existe un cube avec une variable vide, le cube est vide
 *)
let cube_is_empty (cube:cube_t) : bool =
  List.exists literal_is_empty cube
    

let sop_is_empty (sop:sop_t) : bool =
  if sop = [] then true
  else not @@ List.exists (fun x -> not @@ cube_is_empty x) sop
    

let cube_is_universal (cube:cube_t) : bool =
  not (List.exists (List.exists ((=)false)) cube)
    
    
let sop_is_universal (sop:sop_t) : bool =
  List.exists cube_is_universal sop

    
let make_universe (literal:literal_t) : literal_t =
  List.map (fun x -> true) literal
    
    
let make_empty (literal:literal_t) : literal_t =
  List.map (fun x -> false) literal
    
let cube_cofactor (c1:cube_t) (c2:cube_t) =
  (*if cube_intersect c1 c2 |> cube_is_empty then
    List.map (List.map (fun y -> false)) c1
    else*) 
  List.map2 (
    List.map2 (fun a b -> a && (not b)))
    c1 c2
    
    
let sop_cofactor (sop:sop_t) (c:cube_t) : sop_t =
  List.map (fun cube -> cube_cofactor cube c) sop  
      
  
let literal_complement (literal:literal_t) : literal_t =
  List.map (fun x -> not x) literal

    
(* Loi de De Morgan appliqué au produit des littéraux *)
let cube_complement (cube:cube_t) : sop_t =
  let rec aux l previous accu =
    match l with
    | x::r -> aux r (previous@[make_universe x]) ((previous@[literal_complement x]@(List.map make_universe r))::accu)
    | _ -> accu
  in aux cube [] []


let literal_intersect (x1:literal_t) (x2:literal_t) : literal_t =
  List.map2 (fun x y -> x && y) x1 x2

    
let cube_intersect (c1:cube_t) (c2:cube_t) : cube_t =
  List.map2 literal_intersect c1 c2

    
(* (\sum c_i)*(\sum c_j) *)
let sop_intersect (s1:sop_t) (s2:sop_t) : sop_t = 
  List.map (fun c1 ->
    List.map (cube_intersect c1) s2
  ) s1 |> List.flatten |> List.filter (fun x -> not (cube_is_empty x))


let literal_supercube (x1:literal_t) (x2:literal_t) : literal_t =
  List.map2 (fun x y -> x || y) x1 x2


let cube_supercube (c1:cube_t) (c2:cube_t) : cube_t =
  List.map2 literal_supercube c1 c2


let literal_contains (x_in:literal_t) (x:literal_t) : bool =
  not(List.exists2 (fun x y -> not x && y) x_in x)

    
let cube_contains (container:cube_t) (containee:cube_t) : bool =
  not(List.exists2 literal_contains container containee)
    

    
let cube_distance (c1:cube_t) (c2:cube_t) : int =
  let r = cube_intersect c1 c2 in
  List.fold_left (fun a x ->
    if literal_is_empty x then a+1 else a)
    0 r

    
let cube_sharp (s:cube_t) (t:cube_t) : sop_t =
  if cube_intersect s t |> cube_is_empty then [s]
  else sop_intersect [s] (cube_complement t)


let cube_consensus (cube1:cube_t) (cube2:cube_t) : sop_t =
  let rec aux c1 c2 (sop:sop_t) (accu:cube_t) = match c1, c2 with
    | x1::r1, x2::r2 -> aux r1 r2 ((accu@[literal_supercube x1 x2]@(cube_supercube r1 r2))::sop) (accu@[literal_intersect x1 x2])
    | [], [] -> sop
    | _ -> failwith "erreur taille"
  in aux cube1 cube2 [] []


let sop_consensus (sop1:sop_t) (sop2:sop_t) : sop_t =
  sop1 |> List.map (fun c1 ->
    List.map (cube_consensus c1) sop2) |>
      List.flatten |>
	  List.flatten (* TODO: merge ? *)

    
(*   solves the partition problem with the 
 *   number of covered variable as as cost function 
 *)
let partition_glouton (l:(int*int)list) : (int list*int list) =
  let (_,l1,_,l2) = l |> List.fold_left (fun (w1, l1, w2, l2) (i,weight) ->
    if w1 < w2 then (w1+weight, i::l1, w2, l2)
    else (w1, l1, w2+weight, i::l2)
  ) (0, [], 0, [])
  in (l1,l2)
;;

(*
 * Choose the optimal splitting variable 
 * which has the most active values (i.e, 0 parts).
 *)
let choose_splitting_variable (sop:sop_t) : int =
  (* TODO: vérifier s'il s'agit un map ou map_n ici *)
  let var_count_per_cube = list_map_n (List.map (list_count ((=)false))) sop in
  let var_count = list_map_n list_sum var_count_per_cube in
  let (_, j, _) = List.fold_left (fun (i,j,m) x ->
    if x > m then (i+1,i,x) else (i+1,j,m)) (0,0,-1) var_count
  in j
;;
   
let partition (sop:sop_t) : cube_t*cube_t =
  match sop with
  | [] -> failwith "vide"
  | x::r ->
    let model = List.map make_universe x in
    let var_index = choose_splitting_variable sop in
    let values =  List.map (fun x -> List.nth x var_index) sop |>
	list_map_n (list_count ((=)false)) in
    let weights = List.combine (range (List.length values)) values in
    let l1, l2 = partition_glouton weights in
    let rec aux l1 l2 c1 c2 = match l1,l2 with
      | x::r1, y::r2 when x < y ->  aux r1 l2 (true::c1) (false::c2)
      | x::r1, y::r2 -> aux l1 r2 (false::c1) (true::c2)
      | [], [] -> List.rev c1, List.rev c2
      | x::r1, [] -> aux r1 [] (true::c1) (false::c2)
      | [], y::r2 -> aux [] r2 (false::c1) (true::c2)
    in
    (*let f_comp (_,x) (_,y) = if x > y then 1 else if x < y then -1 else 0 in*)
    let c1,c2 = aux (List.sort compare l1) (List.sort compare l2) [] [] in
    let build_var l c = 
      let (_,v) = List.fold_left (fun (i,accu) x ->
	if i <> var_index then (i+1, (x::accu)) else (i+1, c::accu))
	(0,[]) l
      in List.rev v
    in build_var model c1, build_var model c2			       



(* Check whether sop has a universal cube or not *)
let is_tautology_row (sop:sop_t) : bool =
  List.exists cube_is_universal sop


(* Check whether sop has a variable never triggered or not *)
let is_not_tautology_col (sop:sop_t) : bool =
  not(sop |> list_map_n ((* Pour les ensembles de chaque variable de chaque cube *)
    list_map_n (List.exists ((=)true))) |>
      List.map (List.exists ((=)true))
	 |> List.exists ((=)true)
  )                                         (* Alors on est assuré qu'il ne s'agit pas d'une tautologie *)

    
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

  

let sop_irredundant (accepts:cube_t list) (dc:cube_t list) : cube_t list =
  (* construire E_r et R_r *)
  let set = list_union accepts dc in
  let (ens_r_r,ens_e_r) = List.partition (fun cube ->
    sop_contains (List.filter ((<>)cube) set) cube)
    accepts in
  let set_essential = list_union ens_e_r dc in
  let (ens_r_i, ens_r_t) = List.partition (sop_contains set_essential) ens_r_r in
  ens_r_t
  
			     

let sop_essentials (on_set:cube_t list) (dc_set:cube_t list) : cube_t list =
  let set = list_union on_set dc_set in
  List.fold_left (fun l cube ->
    let sharp = set |> List.map (fun x -> cube_sharp x cube) |> List.flatten in
    let h = sop_consensus sharp [cube] in
    if not(sop_contains h cube || sop_contains dc_set cube) then
      cube::l
    else l
  ) [] on_set
  



  
  
	
  
  

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



let espresso_mv on_set dc_set = 
    let off_set = cover_complement (cover_union on_set dc_set) in
    let expanded = cover_irredundant (cover_expand on_set off_set) dc_set in
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
