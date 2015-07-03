open Format
open Str
open Read

type cubeb_t = (bool*bool) list

type sopb_t = cubeb_t list

type bmatrix_t = bool list list


let list_count f l =
  let rec aux l counter = 
    match l with
    | x::r -> if f x then aux r (counter+1) else aux r counter
    | [] -> counter
  in aux l 0


let list_index f set = 
  let rec aux set i = match set with
    | x::r -> if f x then i else aux r (i+1) 
    | [] -> -1
  in aux set 0


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

let sop_from_txt txt : sopb_t= 
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



let contains (c1:cubeb_t) (c2:cubeb_t) = 
  not (List.exists2 (fun (x1,x2) (y1,y2) ->  (not_imply y1 x1) || (not_imply y2 x2)) c1 c2)

let distance (c1:cubeb_t) (c2:cubeb_t) = 
 list_count ((<>)(false,false)) (cube_xor c1 c2)


let merge (c1:cubeb_t) (set:sopb_t) = 
  let rec aux set accu = 
    match set with
    | x::r -> 
      if distance x c1 = 1 then begin 
        print_string "Merge cube ";
        print_cube x;
        print_string " with ";
        print_cube c1;
        print_newline();
        aux r (cube_or c1 x :: accu)
      end else
        aux r accu
    | [] -> if accu = [] then [c1] else accu
  in aux set []


let expand_step (set:sopb_t) = 
  let rec aux set accu prime = 
    match set with
    | x::r -> 
      let merge_set = merge x set in 
      if merge_set = [x] then aux r accu (x::prime) 
      else aux r (merge_set@accu) prime
    | [] -> accu, prime
  in aux set [] []


let expand (set:sopb_t) : sopb_t = 
  let rec aux set accu = 
    let set', prime = expand_step set in
    if set'=[] then prime@accu
    else aux set' (prime@accu)
  in aux set []


let range n =
  let rec aux n accu = match n with
    | 0 -> accu
    | _ -> aux (n-1) (n-1::accu)
  in aux n []


let print_matrix mat = 
  List.iter (fun y -> 
    print_string "[";
    List.iter (fun x -> 
      print_string (if x then " 1 " else " 0 ")
    ) y;
    print_string "]";
    print_newline();
  ) mat


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


let list_pop i l = 
  let rec aux l accu j = 
    match l with 
    | x::r when j<i -> aux r (x::accu) (j+1)
    | x::r when j=i -> (x, List.rev_append accu r)
    | _ -> failwith "erreur non prévue"
  in aux l [] 0


let essentials mat set = 
  let rec aux mat set accu essentials = 
    match mat with 
    | row::r -> 
      if list_count ((=)true) row = 1 then 
        let index = list_index ((=)true) row in
        let e, set' = list_pop index set in
        let f l = 
          let v, row = list_pop index l in 
          if v then [] else row 
        in 
        let mat' =  List.map f r |> List.filter ((<>)[]) in
        let accu =  List.map f accu |> List.filter ((<>)[]) in 
        aux mat' set' accu (e::essentials)

      else aux r set (row::accu) essentials
    | [] -> essentials, List.rev accu, set 
  in aux mat set [] []


let row_dominance mat set = mat, set


let col_contains col1 col2 = 
  not (List.exists2 (fun x y -> not x && y) col1 col2)


let col_dominance (mat:bmatrix_t) (set:sopb_t)  : bmatrix_t*sopb_t= 
  let mat' = transpose mat in
  let rec aux mat set accu_m accu_s = 
    match mat, set with 
    | col::r, x::set' -> 
      if List.exists (fun y -> col_contains y col) r ||
         List.exists (fun y -> col_contains y col) accu_m then 
        aux r set' accu_m accu_s
      else aux r set' (col::accu_m) (x::accu_s)
    | [], [] -> transpose accu_m, accu_s 
    | _ -> 
      failwith "erreur taille col dominance"
  in aux mat' set [] []


let select_score (scores:int list) = 
  let rec aux l i j m = 
    match l with 
    | x::r when x > m -> aux r (i+1) i x
    | x::r -> aux r (i+1) j m 
    | [] -> j
  in aux scores 0 0 (List.hd scores)

let solve_minimum_cover mat set = 
  let rec aux mat set accu = 
    let m' = transpose mat in 
    let scores = List.map (list_count ((=)true)) m' in
    if List.length scores = 0 then accu else begin
    let index = select_score scores in
    let e, set' = list_pop index set in
    let mat' = List.map (fun row -> 
      let v, row' = list_pop index row in if v then [] else row'
    ) mat |> List.filter ((<>)[])
    in aux mat' set' (e::accu) end
  in aux mat set []


let irredundant (on_set:sopb_t) (set:sopb_t) = 
  let mat = List.map (fun cube -> 
    List.map (fun implicant ->
      contains implicant cube
    ) set
  ) on_set in 
  print_matrix mat;
  print_newline();
  
  let rec aux mat set accu = 
    let essential, mat', set' = essentials mat set in
    print_string "essential founds : \n";
    print_sop essential;
    print_newline();
    let mat', set' = row_dominance mat' set' in
    let mat', set' = col_dominance mat' set' in
    match essential with
    | [] -> accu @ (solve_minimum_cover mat set)
    | _ -> aux mat' set' (essential@accu)
  in aux mat set []

let _ = 
  assert (Array.length Sys.argv =2);
  let sop = read_file cube_from_txt Sys.argv.(1) in
  let sop' = expand sop in
  print_sop sop;
  print_newline();
  print_sop sop';
  let irr_sop = irredundant sop sop' in
  print_string "résultats:\n";
  print_sop irr_sop;
  print_newline()

