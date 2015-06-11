(*#use "cover_lib.ml";;*)

(*type cube_t = bool array array;;*)
type literal_t = bool list;;
type cube_t = literal_t list;;
type sop_t = cube_t list;;

type primitive = 
| Literal of literal_t
| Cube of cube_t
| Sop of sop_t;;

let (|>>) (a,b) f =
  f a b
;;

(*
let cube_complement cube =
  let n = Array.length cube in
  let new_cube = Array.make n 
  for i=0 to Array.length cube -1 do
    for j=0 to Array.length cube.(i) -1 do
      cube.(i).(j) <- not cube.(i).(j)
    done
  done
;;

let cube_intersect c1 c2 =
  let n = Array.length c1 in
  assert n = Array.length c2;
  let new_cube = Array.make n false in
  for i=0 to n-1 do
    let m = Array.length c1.(i) in
    assert m = Array.length c2.(i);
    for j=0 to m-1 do
      new_cube.(i).(i) <- c1.(i).(j) && c2.(i).(j)
    done
  done;
  new_cube
;;
*)

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


(* KISS : on choisi la premiere variable qu'on peut prendre *)
let partition_variable (sop:sop_t) =
  let make_partition x =
    let n = (x|> List.filter ((=)true) |> List.length)/2 in
    let rec aux i x accu1 accu2 =
      match x with
      | true::r when i <= n -> aux (i+1) r (true::accu1) (false::accu2)
      | true::r -> aux (i+1) r (false::accu1) (true::accu2)
      | false::r when i <= n -> aux i r (false::accu1) (true::accu2)
      | false::r -> aux i r (true::accu1) (false::accu2)
      | _ -> List.rev accu1, List.rev accu2
    in aux 0 x [] []
  in
  
  let rec aux sop var accu1 accu2 =
    match sop with
    | x::r ->      
      if var=false && x |> List.filter ((=)true) |> List.length >= 2 then
	let (x1, x2) = make_partition x in
	aux r true (x1::accu1) (x2::accu2)
      else
	((x |> List.map (fun y -> true))::accu1, (x |> List.map (fun y -> true))::accu2) |>> (* Les autres variables peuvent prendre des valeurs quelconques *)
	    aux r var
    | _ -> List.rev accu1, List.rev accu2
  in aux sop false [] []
;;

let cube_cofactor (c1:cube_t) (c2:cube_t) =
  if cube_intersect c1 c2 |> cube_is_empty then
    c1 |> List.map (fun x ->
      x|> List.map (fun y -> false
      )
    )
  else
    (c1,c2) |>> List.map2 (fun x y ->
      (x,y) |>> List.map2 (fun a b ->
	a && (not b)
       )
     )
;;

let sop_cofactor (sop:sop_t) (c:cube_t) =
  sop |> List.map (fun cube ->
    cube_cofactor cube c)
;; 


let rec sop_complement (sop:sop_t) =
  match sop with
  | [x] -> cube_complement x
  | [] -> []
  | _ ->
    let (p1, p2) = partition_variable sop in
    let cof1, cof2 = shannon_cofactor sop p1, shannon_cofactor sop p2 in
    sop_union (sop_intersection p1 (sop_complement cof1)) (sop_intersection p2 (sop_complement cof2))
;;

(*
let sop_union sop1 sop2 = 
;;




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

