
type binword_term =
| State of bool
| Wildcare
;;

type binword = binword_term list;;

let string_to_list s =
  let l = ref [] in
  String.iter (fun x -> l := x::!l) s; List.rev !l
;;

let bin_to_binword bin =
  let rec aux accu = function
    | x::r when x='0' -> aux ((State false)::accu) r
    | x::r when x='1' -> aux ((State true)::accu) r
    | x::r when x='-' -> aux (Wildcare::accu) r
    | x::r -> failwith "erreur"
    | _ -> List.rev accu
  in aux [] bin
;;

let hamming_distance a b =
  let rec aux a b n = match (a,b) with
    | (x::r1, y::r2) -> begin match (x,y) with
      | (State u,State v) when u<>v -> aux r1 r2 (n+1)
      | (Wildcare, State u)
      | (State u, Wildcare) -> aux r1 r2 (n+1)
      | _ -> aux r1 r2 n
    end
    | (x::r, _)
    | (_, x::r) -> failwith "Not the same size for evaluating hamming distance"
    | _ -> n
  in aux a b 0
;;

let count l predicat =
  let rec aux n l = match l with
    | x::r -> if (predicat x) then aux (n+1) r else aux n r
    | _ -> n
  in aux l
;;

let is_cover a b =
  let rec aux l1 l2 = match l1,l2 with
    | (Wildcare::r1,_::r2) -> aux r1 r2
    | (_::r1, Wildcare::r2) -> false
    | (State x::r1, State y::r2) when x=y -> aux r1 r2
    | (State x::r1, State y::r2) -> false
    | [],[] -> true
    | _ -> failwith "strange"
  in aux a b
;;
	

let merge_word a b =
  let rec aux a b w = match (a,b) with
    | x::u, y::v when x=y -> aux u v (x::w)
    | x::u, y::v -> aux u v (Wildcare::w)
    | x::u, _
    | _, x::u -> failwith "Pas la même taille"
    | _ -> List.rev w
  in aux a b []
;;


