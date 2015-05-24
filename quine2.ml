open binword;;

let quine accepted_words =

  let rec make_counts accepted_words accu =
    match accepted_words with
    | x::r -> make_counts r ((x,count x)::accu)
    | _ -> accu
  in

  let rec make_groups n l valid unsuitable accu =
    match l with
    | (x,c)::r when c=n -> make_groups n r (x::valid) unsuitable accu
    | (x,c)::r -> make_groups n r valid (x::unsuitable) accu
    | _ ->
      if unsuitable <> [] then
	make_groups (n+1) unsuitable [] [] (valid::accu)
      else List.rev (valid::accu) (* valid <> [] dans tous les cas si on est ici *)
  in
    
  (*
   * @ x : mot du groupe inférieur à fusionner
   * @ l : liste des mots du groupe supérieur
   * @ merged : nouveaux mots créés ( [] pour utilisation normale )
   * @ matched : mot du groupe supérieur qui ont étés utilisés ( [] pour utilisation normale )
   *)
  let rec merge x l merged matched  =
    match l with
    | y::r ->
      if hamming_distance x y = 1 then
	merge x r (merge_word x y) (y::matched) 
      else
	merge x r merged matched
    | _ -> merged matched
  (*
   * Effectue l'opération de fusion entre deux groupes.
   * @ l1 :
   * @ l2 :
   * @ matched : 
   * @ unmatched : 
   *)
  let rec aux l1 l2 matched unmatched =
    match l1 with
    | x::r ->
      let x_merged, x_matched = merge x l2 [] [] in
      
      
    | _ -> (List.filter () matched), unmatched)
  in

  (*


  *)
  let rec step_quine groups merged =
    match groups with
    | x::y::r ->
      let (first_group, merged_group) = aux x y [] [] in
      step_quine step_quine (y::r) (first_group::merged_group::merged)
      
  in
  
