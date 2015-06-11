(*#use "binword2.ml"
*)


let attrib_cover (x:binword) mintermes = 
  List.filter (is_cover x) mintermes
;;

let coverture (implicants:binword list) (mintermes:binword list) = 
  List.map (fun x -> (x, attrib_cover x mintermes)) implicants
;;

let find_max_cover cover_set =
  let rec aux cover_set max n = 
    match cover_set with
    | (x,l)::r ->
      let m = List.length l in
      if m > n then aux r (Some (x,l)) m
      else aux r max n
    | _ -> max
  in aux cover_set None 0
;;

let extract_essential cover_set = 
  let rec aux cover_set accu unsuitable covered= 
    match cover_set with
    | (x,l)::r ->
      begin match l with
      | [y] -> aux r (x::accu) unsuitable (y::covered)
      | _   -> aux r accu ((x,l)::unsuitable) covered
      end 
    | _ -> accu, unsuitable, covered
  in
  let (prime_essentiels, unsuitable, covered) = aux cover_set [] [] [] in
  (prime_essentiels, List.map (
    fun (x,l) -> (
      x, List.filter (fun y -> List.mem y covered) l)
   ) unsuitable
  )
;;
	
let compute_mincover implicants mintermes = 
  let minterm_cover = coverture implicants mintermes in
  let essentials, cover = extract_essential minterm_cover in
    (* TODO: filtrer impliquant premier *)
  let rec aux m_cover accu = 
    if m_cover = [] then accu
    else begin
      let next_implicant = find_max_cover m_cover in
      match next_implicant with
      | None -> accu
      | Some (x,implique) ->
	   (* SHORTCUT TRICK *)
	   (* on filtre m_cover pour supprimer l'impliquant qu'on a enlevé *)
	   (* puis on supprime les mintermes déjà couvert *)
	let new_cover_set = List.filter (fun (z,_) -> z<>x) m_cover in
        let new_cover_set2 = List.map 
          (fun (y,l) ->
	    (y, List.filter (fun z -> not (List.mem z implique)) l)
	  ) new_cover_set 
	in aux new_cover_set2 (x::accu)
    end
  in aux cover essentials
;;
