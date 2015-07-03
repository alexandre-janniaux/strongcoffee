open Tools
     
let expand_product product = 
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
  in aux product [[]]


let min_sum_weight (sum:int list list) set = 
  let scores = List.map (fun prod ->
    let cube_costs = 
      List.map (fun index -> cube_cost (List.nth set index)) prod 
    in list_sum (fun x->x) cube_costs
  ) sum in
  select_score scores


let petrick_covering on_set set = 
  let implicant_marked = List.combine (range (List.length set)) set in
  let prod_sum = List.map (fun cube -> 
    List.map (fun (i,implicant) -> (i, contains implicant cube)) implicant_marked
    |> List.filter (fun (i,v)->v) 
    |> List.map (fun (i,_) -> i))
    on_set in
  let sum = expand_product prod_sum in
  let index = min_sum_weight sum set in
  List.map (List.nth set) (List.nth sum index)


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



    

  

