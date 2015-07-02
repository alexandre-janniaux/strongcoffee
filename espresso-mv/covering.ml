open MultipleValued

type matrix_t = bool list list

let extract l set = 
  let rec aux l set accu = 
    match l, set with
    | false::lr, x::xr -> aux lr xr (x::accu) 
    | true::lr, x::xr -> (x, List.rev_append accu xr)
    | _ -> failwith "erreur"
  in aux l set []


let list_pop l i = 
  let rec aux l accu j = 
    match l with 
    | x::r when j<i -> aux r (x::accu) (j+1)
    | x::r when j=i -> (x, List.rev_append accu r)
    | _ -> failwith "erreur non prévue"
  in aux l [] 0



(* TODO: remove covered rows *)
let extract_essentials matrix set =
  let rec aux matrix set essential final_set accu = match matrix with
  | l::r -> 
    if list_count ((=)true) l = 1 then
      let e, set' = extract l set in
        aux r set' (e::essential) final_set accu
      else
        aux r set essential (final_set) (l::accu)
  | _ -> essential, List.rev accu, List.rev final_set 
  (* Accessoirement, on peut faire sans rev les deux, mais il faut garder le même ordre derrière *)
  in aux matrix set [] [] []





let list_contains l1 l2 = 
  not(List.exists2 (fun x y -> not x && y) l1 l2)


let list_cut l n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> 
      if i = 0 then List.rev acc, l
      else aux (i-1) (h :: acc) t
  in aux 0 [] l 



let remove_row_dominance matrix =
  let rec aux matrix accu = 
    match matrix with 
    | row::r -> 
      if List.exists (list_contains row) r then 
        aux r accu
      else
        let m' = List.filter (fun x -> list_contains x row) r in
        aux m' (row::accu)
    | [] -> List.rev accu
  in aux matrix []




let remove_colum_dominance matrix set = 
  let transposed = transpose matrix in (* on se ramène à la situation avec des lignes *)
  let rec aux matrix set accu = 
    match matrix with 
    | col::r -> 
      if List.exists (fun x -> list_contains x col) r then 
        aux r set accu
      else
        let m' = List.filter (list_contains col) r in
        aux m' set (col::accu)
    | [] -> List.rev accu, set
  in 
  let m', set' = aux transposed set [] in
  transpose m', set'




let compute_score rows = 
  let rec aux rows accu = 
    match rows with
    | row::r -> aux r ((list_count ((=)true) row)::accu)
    | [] -> List.rev accu
  in aux rows []



let select_score f_compare scores =
  let rec aux rows best i i_best = 
    match rows with
    | x::r -> 
      if f_compare x best then 
        aux r x (i+1) i
      else aux r best (i+1) i_best
    | [] -> i_best
  in aux scores (List.hd scores) 0 0



let max_independant_set matrix = 
  let rec aux matrix accu = 
    match matrix with 
    | [] -> accu
    | _ -> 
      let scores = compute_score matrix in 
      let best = select_score (<) scores in
      let row, m' = list_pop matrix best in
      let m' =  List.filter (fun x -> literal_intersect x row |> literal_is_empty ) m' in
      aux m' (row::accu)
  in aux matrix []



let select_column matrix independant_set =  
  let scores = compute_score matrix in 
  let weights = List.map2 (fun score row -> 
    let s = 1. /. (float_of_int score) in
    List.map (function true -> s | false -> 0.) row
  ) scores matrix in
  let m' = transpose weights in 
  let values = List.map list_sumf m' in 
  let best = select_score (>) values in
  best


let matrix_reduce matrix k = 
  matrix


let matrix_remove matrix k = 
  list_pop matrix k |> snd




let minimum_cover matrix set best = 
  let rec aux matrix (set:sop_t) best accu = 
    (* STEP (2) remove essential elements *)
    let essential, matrix, set = extract_essentials matrix set in

    (* STEP (3) apply row dominance *)
    let matrix = remove_row_dominance matrix in

    (* STEP (4) apply column dominance *)
    let matrix, set = remove_colum_dominance matrix set in

    (* STEP (5) check if problem is solved *)
    if matrix = [] then
      List.rev_append accu essential

    (* STEP (6) otherwise, branch&bound algorithm to reach the solution *)
    else 
      let max_set = max_independant_set matrix in 
      (*if list_union essential max_set*)
      let split_column = select_column matrix max_set in
      let q, set' = list_pop set split_column in

      let matrix_l = matrix_reduce matrix split_column 
      and matrix_r = matrix_remove matrix split_column in

      let left = aux matrix_l set' best (accu@q::essential) in 
      let best = if sop_cost left > sop_cost best then best else left in
      
      let right = aux matrix_r set' best (accu@q::essential) in
      if sop_cost right > sop_cost best then best else left

  in aux matrix set [] set


let solve_minimum_cover matrix (set:sop_t) = 
  minimum_cover matrix set set
