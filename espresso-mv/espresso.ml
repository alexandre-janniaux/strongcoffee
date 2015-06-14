(*#use "cover_lib.ml";;*)

type literal_t = bool list;;
type cube_t = literal_t list;;
type sop_t = cube_t list;;

type primitive = 
| Literal of literal_t
| Cube of cube_t
| Sop of sop_t;;

(* douple pipe operator *)
let (|>>) (a,b) f =
  f a b
;;

(* Loi de De Morgan appliqué au produit des littéraux *)
let cube_complement (cube:cube_t) : sop_t =
  cube |> List.map (fun l ->
    [l |> List.map (fun x -> not x)] (* Devient un SOP *) 
  )
;;

(*
 * S'il existe un cube avec une variable vide, le cube est vide
 *)
let cube_is_empty (cube:cube_t) : bool =
  cube |> List.exists (fun x ->
    not (x |> List.exists ((=)true))
  )
;;

let sop_is_empty (sop:sop_t) : bool =
  if sop = [] then true
  else sop |> List.fold_left (fun a x -> a && cube_is_empty x) true
;;

(*
 * Si pour tous les cubes, les variables sont une tautologie, alors
 * le cube est universel
 *)
let cube_is_universal (cube:cube_t) : bool =
  not (cube |> List.exists (fun x ->
    x |> List.exists ((=)false)
  ))
;;

let cube_intersect (c1:cube_t) (c2:cube_t) : cube_t =
  (c1, c2) |>> List.map2 (fun l1 l2 ->
    (l1, l2) |>> List.map2 (fun x y -> x && y)
  )
;;

let sop_intersect (s1:sop_t) (s2:sop_t) : sop_t = 
  s1 |> List.map (fun c1 ->
    s2 |> List.map (fun c2 ->
      cube_intersect c1 c2
    )) |> List.flatten
;;
    
  
let cube_supercube (c1:cube_t) (c2:cube_t) : cube_t =
  (c1, c2) |>> List.map2 (fun l1 l2 ->
    (l1, l2) |>> List.map2 (fun x y -> x || y)
  )
;;

let cube_contains (container:cube_t) (containee:cube_t) : bool =
  not( (container, containee) |>> List.exists2 (fun a b ->
    not ( (a,b) |>> List.exists2 (fun x y -> 
      not (x || (not y))
    ))
  ))
;;

let cube_distance (c1:cube_t) (c2:cube_t) : int =
  let r = cube_intersect c1 c2 in
  r |> List.fold_left (fun a x ->
    if not (List.exists ((=)true) x) then a else a+1
  ) 0
;;

let sop_contains (container:sop_t) (containee:sop_t) : bool =
  not(containee |> List.exists (fun x ->
    not (container |> List.exists (fun y ->
      cube_contains y x
    ))
  ))
;;

let cube_sharp (s:cube_t) (t:cube_t) : sop_t =
  if cube_intersect s t |> cube_is_empty then [s]
  else sop_intersect [s] (cube_complement t)
;;

let make_universe (literal:literal_t) : literal_t =
    literal |> List.map (fun x -> true)
;;

let make_null (literal:literal_t) : literal_t =
    literal |> List.map (fun x -> false)
;;

let choose_splitting_variable sop =
  let (_, var) =
    (* iterate over all cubes to find the variable *)
    sop |> List.fold_left (fun (m,var) cube ->
      (* iterate over all variable and build the optimal one recursively *)
      cube |> List.fold_left (fun (m,var) literal ->
	let n = literal |> List.filter ((=)false) |> List.length in
	if m < n then (* if we found a new optimal var, resets the former cube
			 and concatenates it with the new one *)
	  (n, (make_null literal)::(List.map make_universe var))
	else (* otherwise, clear the variable and push it at the top *)
	  (m, (make_universe literal)::var)
      ) (-1, []) (* cube *)
    ) (-1, []) (* sop *)
  in List.rev var (* cube has been built reversly*)
;;



(*   make_partition solves the partition problem with the 
 *   number of covered variable as as cost function 
 *)
let make_partition (sop:sop_t) (var:cube_t) : (cube_t*cube_t) =
  let compare =
    sop |> List.map (fun cube ->
      (cube, var) |>> List.fold_left (fun a x y ->
	if List.filter ((=)0) var = [] then a
	else x
       ) [])
  in 
	  
  ([],[])
;;

  (*
  in let split_variable var sop =
     (* iterate over all cubes *)
       sop |> List.fold_left (fun a cube ->
	 (* iterate over all variable *)
	 cu
     in
       
  in choose_splitting_variable sop
;;
  *)
  

let cube_cofactor (c1:cube_t) (c2:cube_t) =
  if cube_intersect c1 c2 |> cube_is_empty then
    c1 |> List.map (fun x ->
      x|> List.map (fun y -> false
      ))
  else
    (c1,c2) |>> List.map2 (fun x y ->
      (x,y) |>> List.map2 (fun a b ->
	a && (not b)
       ))
;;

let sop_cofactor (sop:sop_t) (c:cube_t) =
  sop |> List.map (fun cube ->
    cube_cofactor cube c)
;;

(*
let rec sop_complement (sop:sop_t) =
  match sop with
  | [x] -> cube_complement x
  | [] -> []
  | _ ->
    let (p1, p2) = partition sop in
    let cof1, cof2 = shannon_cofactor sop p1, shannon_cofactor sop p2 in
    sop_union (sop_intersection p1 (sop_complement cof1)) (sop_intersection p2 (sop_complement cof2))
  ;;*)

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
