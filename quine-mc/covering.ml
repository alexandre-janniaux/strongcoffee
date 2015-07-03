open Tools
open Format
open Benchmark


let print_product prod = 
  let rec aux_sum sum = 
    match sum with
    | x::r -> 
      print_int x;
      if r <> [] then print_string "+";
      aux_sum r
    | [] -> ()

  and aux_prod prod =
    match prod with
    | x::r ->
      print_string "(";
      aux_sum x;
      print_string ")";
      aux_prod r
    | [] -> ()
  in aux_prod prod


let print_sum sum = 
  let rec aux_sum sum = 
    match sum with 
    | x::r -> 
      print_string "(";
      aux_prod x;
      print_string ")";
      if r <> [] then print_string "+";
      aux_sum r
    | [] -> ()

  and aux_prod prod = 
    match prod with
    | x::r ->
      print_int x;
      if r <> [] then print_string ".";
      aux_prod r
    | [] -> ()

  in aux_sum sum


(*
 *    Développpe totalement le produit en somme.
 *    ATTENTION : peut provoquer un stack overflow
 *    sur de grandes instances.
*)     
let expand_product product = 
  let bench = start_record "expand_product" "" in
  let rec expand1_product x l accu =
    match l with
    | prod::r -> 
      if List.mem x prod then 
        expand1_product x r (prod::accu)
      else
        expand1_product x r ((x::prod)::accu)
    | [] -> accu 
  in

  let rec aux extprod accu = 
    (*
     *  Accu va stocker ici l'ensemble des produits de la sommme
     *  déjà constituée.
     *)
    match extprod with
    | sum::r -> 
      aux r ((List.map (fun x -> expand1_product x accu []) sum) |> List.flatten)
    | [] -> accu
  in aux product [[]] =@ bench


let min_sum_weight (sum:int list list) set = 
  let bench = start_record "min_sum_weight" "" in
  let scores = List.map (fun prod ->
    let cube_costs = 
      List.map (fun index -> cube_cost (List.nth set index)) prod 
    in list_sum (fun x->x) cube_costs
  ) sum in
  select_score (<) scores =@ bench


(*   
 *   Tente de calculer le set impliquant le moins cher en applicant une 
 *   méthode de backtracking
*)
let expand_backtrack product set = 
  let bench = start_record "expand_backtrack" "" in
  let rec aux product cost solution best_cost best = 
    match product with
    | (x::sum)::r -> 
      (*    
       *    Cas de base : 
       *    si x est déjà dans la solution, on va évidemment le choisir 
       *    sans changer le coût de la solution
       *)
      if List.mem x solution then aux r cost solution best_cost best else
        (* que nous coûterait l'ajout de x dans la solution ? *)
        let put_cost = cube_cost (List.nth set x) in 
        begin
          match best_cost with
          | None ->
            (* On est sur la première branche, même cas qu'en dessous *)
            let branch_cost, branch_best = 
              aux r (cost+put_cost) (x::solution) best_cost best in
            aux (sum::r) cost solution branch_cost branch_best

          | Some c when (c > x+put_cost) -> 
            (* On a pas encore atteint un maximum, donc on continue de développer *)
            let branch_cost, branch_best = 
              aux r (cost+put_cost) (x::solution) best_cost best in
            aux (sum::r) cost solution branch_cost branch_best

          | Some c -> 
            (* On a déjà trouvé un meilleur produit *)
            aux (sum::r) cost solution best_cost best
        end
    | []::r -> 
      best_cost, best
    | [] -> 
      (* Si on arrive au bout d'une branche, on a atteint un maximum qui 
       * est meilleur que celui qu'on avait. *)
      match best_cost with 
      | None -> Some cost, Some solution
      | Some c -> 
        if cost < c then 
          Some cost, Some solution
        else 
          best_cost, best
  in let _,solution = aux product 0 [] None None =@ bench in
  match solution with
  | None -> [] (* Pas de solution ? c'est qu'on a rien dans le produit *)
  | Some l -> l


(*
 *    Trouve le plus petit sous-set de set qui couvre totalement on_set
 *)
let petrick_covering on_set set backtracking = 
  let bench = start_record "petrick_covering" "" in
  let implicant_marked = List.combine (range (List.length set)) set in
  let prod_sum = List.map (fun cube -> 
    List.fold_left (fun accu (i,implicant) -> if contains implicant cube then i::accu else accu)
      [] implicant_marked)
    on_set in
  if not backtracking then
    let sum = expand_product prod_sum in
    let index = min_sum_weight sum set in
    List.map (List.nth set) (List.nth sum index) =@ bench
  else
    let exact_set = expand_backtrack prod_sum set in
    List.map (List.nth set) exact_set =@ bench


    

  

