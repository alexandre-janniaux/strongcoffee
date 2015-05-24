
type binword_term =
| State of bool
| Wildcare
;;

type binword = binword_term list;;

let bin_to_bin_word bin =
  let rec aux accu = function
    | x::r when x=0 -> aux ((State false)::accu) r
    | x::r when x=1 -> aux ((State true)::accu) r
    | x::r -> failwith "erreur"
    | _ -> List.rev accu
  in aux [] bin
;;

let hamming_distance a b compare =
  let rec aux a b n = match (a,b) with
    | (x::r1, y::r2) when (compare x y)=false -> aux r1 r2 (n+1)
    | (x::r1, y::r2) -> aux r1 r2 n
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

