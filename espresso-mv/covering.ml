open MultipleValued

type matrix_t = bool list list

let extract l set = 
  let rec aux l set accu = 
    match l, set with
    | false::lr, x::xr -> aux lr xr (x::accu) 
    | true::lr, x::xr -> (x, List.rev_append accu xr)
    | _ -> failwith "erreur"
  in aux l set []
       

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



let solve_minimum_cover matrix (set:sop_t) = 

  let rec aux matrix (set:sop_t) accu = 
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
      accu
  in aux matrix set []

