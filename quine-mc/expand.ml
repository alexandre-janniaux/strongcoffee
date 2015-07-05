open Format
open Str
open Read
open Tools
open Benchmark


(*
 *    Tente de fusionner le cube avec chaque élément
 *    de l'ensemble set, et renvoie les éléments
 *    fusionnés. S'il n'y en a aucun, renvoie le 
 *    cube.
 *)
let merge (c1:cubeb_t) (set:sopb_t) = 
  let rec aux set accu = 
    match set with
    | x::r -> 
      if distance x c1 = 1 then begin 
        (*print_string "Merge cube ";
        print_cube x;
        print_string " with ";
        print_cube c1;
        print_newline();*)
        aux r (cube_or c1 x :: accu)
      end else
        aux r accu
    | [] -> if accu = [] then [c1] else accu
  in aux set []

(*
 *    Effectue une étape d'expansion entre tous les 
 *    termes.
 *)
let expand_step (set:sopb_t) = 
  let rec aux set accu prime = 
    match set with
    | x::r -> 
      let merge_set = merge x set in 
      if merge_set = [x] then aux r accu (x::prime) 
      else aux r (merge_set@accu) prime
    | [] -> accu, prime
  in 
  aux set [] []


(*
 *    Applique l'algorithme d'expansion
 *    tant que tous les implicants premiers
 *    n'ont pas été trouvés.
 *)
let expand (set:sopb_t) : sopb_t = 
  let bench = start_record "expand" "" in
  let rec aux set accu = 
    let set', prime = expand_step set in
    if set'=[] then prime@accu
    else aux set' (prime@accu)
  in aux set [] =@ bench


let print_matrix mat = 
  List.iter (fun y -> 
    print_string "[";
    List.iter (fun x -> 
      print_string (if x then " 1 " else " 0 ")
    ) y;
    print_string "]";
    print_newline();
  ) mat


(*
 *    Equivalent de List.map, mais avec un nombre de liste
 *    arbitraire en paramètre.
 *)
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
    | _ -> failwith "erreur non prévue avec list_pop"
  in aux l [] 0


let essentials mat set = 
  (*
   *    La fonction essentials cherche des lignes avec un seul 1,
   *    qui indique la présence d'un implicant essentiel.
   *    En effet, si une ligne n'a qu'un seul 1, alors
   *    il n'y a qu'une seule possibilité de couvrir le
   *    terme.
   *)
  let bench = start_record "essentials" "" in
  let rec aux mat set accu essentials = 
    match mat with 
    | row::r -> 
      if list_count ((=)true) row = 1 then begin
        let index = list_index ((=)true) row in
        let e, set' = list_pop index set in
        (*
         *    On va pouvoir supprimer toutes les autres lignes
         *    que l'implicant essentiel couvre
         *)
        let f accu l = 
          let v, row = list_pop index l in 
          if v then accu else row::accu
        in 
        let mat' =  List.rev @@ List.fold_left f [] r  in
        let accu' =  List.rev @@ List.fold_left f [] accu in 
        aux mat' set' accu' (e::essentials)

      end else aux r set (row::accu) essentials
    | [] -> 
      (* Il n'y a pas besoin de renverser les colonnes *)
      essentials, accu, set 
  in aux mat set [] [] =@ bench


let row_contains col1 col2 = 
  not (List.exists2 (fun x y -> not x && y) col1 col2)


let row_dominance mat = 
  (*
   *    On applique row_dominance pour supprimer les lignes
   *    qui en contiennent d'autres : 
   *    si une telle ligne existe c'est qu'un des implicants est plus grand 
   *    que les autres, et va forcément apparaitre dans la solution.
   *)
  let bench = start_record "row_dominance" "" in
  let rec aux mat accu = 
    match mat with
    | row::r -> 
      if List.exists (row_contains row) r ||
         List.exists (row_contains row) accu
      then aux r accu
      else aux r (row::accu)
    | [] -> 
      (* L'ordre n'a pas d'importance, on a manipulé les lignes *)
      accu
  in aux mat [] =@ bench



let col_dominance (mat:bmatrix_t) (set:sopb_t)  : bmatrix_t*sopb_t= 
  (*
   *    On applique col_dominance pour supprimer les implicants
   *    plus petit : clairement, ci C_1 est contenu dans C_2
   *    alors tous les termes de C_1 sont couverts par C_2
   *)
  let bench = start_record "col_dominance" "" in 
  let mat' = transpose mat in

  let rec aux mat set accu_m accu_s = 
    match mat, set with 
    (*| [row], [x] -> [row], [x]*)
    | col::r, x::set' -> 
      if List.exists (fun y -> row_contains y col) r ||
         List.exists (fun y -> row_contains y col) accu_m 
      then 
        (* On supprime la colonne et l'impliquant, on en a trouvé un plus gros *)
        aux r set' accu_m accu_s
      else 
        (* Sinon on la garde, en la plaçant à la même position que l'implicant *)
        aux r set' (col::accu_m) (x::accu_s)
    | [], [] -> 
      (* Il n'y a pa besoin de s'inquiéter de l'ordre, car les implicants et les 
       * colones ont été permutées simultanément *)
      accu_m, accu_s  
    | _ -> 
      (* Si on a pas le même nombre de colonne que d'implicant, c'est qu'on en a
       * perdu en route et qu'il y a un défaut de fonctionnement *)
      printf "mat_size : %i / set_size : %i\n" (List.length mat) (List.length set);
      failwith "[col_dominance] Erreur : le nombre de colonne est différent du nombre d'implicant."

  in 
  let mat'', set' = aux mat' set [] [] in 
  (transpose mat'', set') =@ bench

(*
 *  Résout le problème de couverture minimale d'ensemble
 *  étant donné la matrice de couverture.
 *)
let solve_minimum_cover mat set = 
  let bench = start_record "solve_minimum_cover" "" in
  let rec aux mat set accu = 
    let m' = transpose mat in 
    let scores = List.map (list_count ((=)true)) m' in

    if List.length scores = 0 then accu else begin
      let index = select_score (>) scores in
      let e, set' = list_pop index set in
      let mat' = List.map (fun row -> 
        let v, row' = list_pop index row in if v then [] else row'
      ) mat |> List.filter ((<>)[])
      in aux mat' set' (e::accu) 
    end
  in aux mat set [] =@ bench


(*
 *    Cherche un sous-ensemble minimal (pas forcément le minimum)
 *    de set qui recouvre on_set
 *)
let irredundant (on_set:sopb_t) (set:sopb_t) = 
  let on_set_size = istr@@ List.length on_set
  and set_size = istr@@ List.length set in
  let bench = start_record "irredundant" ("on_set_size:"^on_set_size^"; set_size:"^set_size) in
  let mat = List.map (fun cube -> 
    List.map (fun implicant ->
      contains implicant cube
    ) set
  ) on_set in 
  
  let rec aux mat set accu = 
    let essential, mat', set' = essentials mat set in
    let mat' = row_dominance mat' in
    let mat', set' = 
      if List.length mat' > 0 then col_dominance mat' set' 
      else [], []
    in

    match essential with
    | [] -> accu @ (solve_minimum_cover mat set)
    | _ -> aux mat' set' (essential@accu)
  in aux mat set [] =@ bench
       


(*
 *    Vérifie que chaque cube élémentaire est inclus dans 
 *    un des cubes de l'ensemble set.
 *)
let sop_verify (on_set:sopb_t) (set:sopb_t) =
  try let cube = List.find (fun cube -> 
    not (
      List.exists (fun implicant -> contains implicant cube) set
    ))
    on_set in 
    print_cube cube; false
  with 
  | Not_found -> true
    
