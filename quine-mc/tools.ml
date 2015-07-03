open Format
open Str

type cubeb_t = (bool*bool) list

type sopb_t = cubeb_t list

type bmatrix_t = bool list list



let apply_cubes f (c1:cubeb_t) (c2:cubeb_t) =
  let rec aux c1 c2 accu = 
    match c1, c2 with
    | (x1,x2)::r1, (y1,y2)::r2 -> aux r1 r2 ((f x1 y1,f x2 y2)::accu)
    | [], [] -> List.rev accu
    | _ -> failwith "erreur taille"
  in aux c1 c2 []


let not_imply x y = x && (not y)


let cube_intersect = apply_cubes (&&)

let cube_or =  apply_cubes (||)

let cube_xor = apply_cubes (fun x y -> (x || y) && not(x&&y))

let list_count f l =
  let rec aux l counter = 
    match l with
    | x::r -> if f x then aux r (counter+1) else aux r counter
    | [] -> counter
  in aux l 0

let list_sum f l = 
  let rec aux l sum = 
    match l with
    | x::r -> aux r (sum+(f x))
    | [] -> sum
  in aux l 0


let list_index f set = 
  let rec aux set i = match set with
    | x::r -> if f x then i else aux r (i+1) 
    | [] -> -1
  in aux set 0


let contains (c1:cubeb_t) (c2:cubeb_t) = 
  not (List.exists2 (fun (x1,x2) (y1,y2) ->  (not_imply y1 x1) || (not_imply y2 x2)) c1 c2)

let distance (c1:cubeb_t) (c2:cubeb_t) = 
 list_count ((<>)(false,false)) (cube_xor c1 c2)



let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


let cube_from_txt txt = 
  let vars = Str.split (Str.regexp " ") txt in
  List.map (function
    | [x;y] -> let f = fun x -> if x='1' then true else false in (f x, f y)
    | _ -> failwith "erreur lecture")
    (List.map explode vars)

let sop_from_txt txt = 
  List.map cube_from_txt txt

let cube_to_str cube =
  let str = ref "[" in 
  List.iter (fun (x,y) -> 
    let f x = if x then  "1" else "0" in
    str := !str ^ f x ^ f y ^ " "
  ) cube;
  (!str) ^ "]" 


let print_cube cube =
  print_string (cube_to_str cube)

let print_sop sop = 
  List.iter (fun cube ->
    print_string (cube_to_str cube);
    print_newline()
  ) sop

let range n =
  let rec aux n accu = match n with
    | 0 -> accu
    | _ -> aux (n-1) (n-1::accu)
  in aux n []

let cube_from_bitstr txt = 
  let vars = explode txt in
  List.map (function
    | '0' -> (true,false)
    | '1' -> (false,true)
    | '~' -> (true,true)
    | _ -> failwith "Erreur parsage") vars

let cube_cost cube = 
  list_count ((<>)(true,true)) cube

let sop_cost sop = 
  list_sum (fun x->x) 
    (List.map (list_count ((<>)(true,true))) sop)

let select_score f_compare (scores:int list) = 
  let rec aux l i j m = 
    match l with 
    | x::r when f_compare x m -> aux r (i+1) i x
    | x::r -> aux r (i+1) j m 
    | [] -> j
  in aux scores 0 0 (List.hd scores)


