open Format 

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

let list_sumf l =
  List.fold_left (fun a x -> x+.a) 0. l

let list_enumerate f (l:'a list) = 
  let rec aux l i = 
    match l with
    | x::r -> f i x; aux r (i+1)
    | _ -> ()
  in aux l 0


let list_map_n (f:'a list -> 'b) (l:'a list list) : 'b list =
  let rec depile l = (List.map List.hd l, List.map List.tl l |> List.filter ((<>)[])) in
  let rec aux l accu =
    match l with
    | [] -> List.rev accu
    | _ ->
      let (t,r) = depile l in
      aux r ((f t)::accu)
  in aux l []

let transpose (matrix:'a list list) : 'a list list = 
  list_map_n (fun col -> col) matrix


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


let literal_is_universal(x:literal_t) : bool =
  not (List.exists ((=)false) x)


let cube_is_universal (cube:cube_t) : bool =
  not (List.exists (fun x -> not (literal_is_universal x)) cube)
    
    
let sop_is_universal (sop:sop_t) : bool =
  List.exists cube_is_universal sop

    
let make_universe (literal:literal_t) : literal_t =
  List.map (fun x -> true) literal
    
    
let make_empty (literal:literal_t) : literal_t =
  List.map (fun x -> false) literal

let literal_contains (x_in:literal_t) (x:literal_t) : bool =
  not(List.exists2 (fun x y -> not x && y) x_in x)

    
let cube_contains (container:cube_t) (containee:cube_t) : bool =
  not(List.exists2 literal_contains container containee)
    

let sop_filter2 set =
  let rec aux set accu = match set with
  | x::r -> if List.exists (fun y -> cube_contains y x) (r@accu) then aux r accu else aux r (x::accu)
  | [] -> accu
  in aux set []


let sop_filter (sop:sop_t) : sop_t = 
  let rec aux sop accu = match sop with
    | [x] when accu = [] -> [x]
    | x::r -> 
      if cube_is_empty x then aux r accu 
      else if cube_is_universal x then [x]
      else aux r (x::accu)
    | _ -> List.rev accu
  in aux sop []

let sop_filter_double (sop:sop_t) : sop_t =
  let rec aux sop accu = match sop with 
    | x::r -> if List.mem x r then aux r accu else aux r (x::accu)
    | [] -> accu
  in aux sop []    



let literal_intersect (x1:literal_t) (x2:literal_t) : literal_t =
  List.map2 (fun x y -> x && y) x1 x2

    
let cube_intersect (c1:cube_t) (c2:cube_t) : cube_t =
  List.map2 literal_intersect c1 c2

    
(* (\sum c_i)*(\sum c_j) *)
let sop_intersect (s1:sop_t) (s2:sop_t) : sop_t = 
  List.map (fun c1 ->
    List.map (cube_intersect c1) s2
  ) s1 |> List.flatten |> sop_filter |> sop_filter_double |> sop_filter2



let cube_distance (c1:cube_t) (c2:cube_t) : int =
  let r = cube_intersect c1 c2 in
  list_count literal_is_empty r


let cube_cofactor (c1:cube_t) (c2:cube_t) =
  if cube_distance c1 c2 <> 0 then
    List.map (List.map (fun y -> false)) c1
  else
  List.map2 (
    List.map2 (fun a b -> a || (not b)))
    c1 c2
    
    
let sop_cofactor (sop:sop_t) (c:cube_t) : sop_t =
  List.map (fun cube -> cube_cofactor cube c) sop |> sop_filter |> sop_filter_double
      
  
let literal_complement (literal:literal_t) : literal_t =
  List.map (fun x -> not x) literal

    
(* Loi de De Morgan appliqué au produit des littéraux *)
let cube_complement (cube:cube_t) : sop_t =
  let rec aux l previous accu =
    match l with
    | x::r -> aux r (previous@[make_universe x]) ((previous@[literal_complement x]@(List.map make_universe r))::accu)
    | _ -> accu
  in aux cube [] [] |> sop_filter


let literal_supercube (x1:literal_t) (x2:literal_t) : literal_t =
  List.map2 (fun x y -> x || y) x1 x2


let cube_supercube (c1:cube_t) (c2:cube_t) : cube_t =
  if cube_is_empty c1 then 
    c2
  else if cube_is_empty c2 then
    c1
  else
    List.map2 literal_supercube c1 c2


let sop_supercube (sop:sop_t) : cube_t = match sop with
| [x] -> x
| x::t -> List.fold_left cube_supercube x t
| [] -> []

    
    
let cube_diff (c1:cube_t) (c2:cube_t) : cube_t = 
  List.map2 
    (List.map2 (fun x y -> x && (not y)))
    c1 c2


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
  sop1 |> 
  List.map (fun c1 -> List.map (cube_consensus c1) sop2) |>
  List.flatten |>
  List.flatten (* TODO: merge ? *)


let sop_cost (sop:sop_t) = 
  List.length sop



