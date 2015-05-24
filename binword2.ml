
type binword = int*int (* value * mask *)

let hamming_distance a b =
    let b = ref 1 
    and c = a lxor b in
    let rec aux c n = 
      if c=0 then n 
      else if c land 1 then aux (c lsr 1) (n+1)
      else aux (c lsr 1) n
    in aux c 0
;;

let merge_word a b = 
  let (n1, m1) = a and (n2, m2) = b in
  if m1 <> m2 then failwith "Pas les mêmes masques"
  else (n1 land m1, (n1 lxor n2) lor m1)
;;


(*let count l predicat =
  let rec aux n l = match l with
    | x::r -> if (predicat x) then aux (n+1) r else aux n r
    | _ -> n
  in aux l
;;
 *)
