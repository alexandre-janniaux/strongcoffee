open Printf;;

type binword = int*int (* value * mask *)

let hamming_distance (a,m1) (b,m2) =
  let c = a  lxor b in
  let m = m1 lxor m2 in
  let rec aux c m n = 
    if c=0 then n 
    else if c land 1 <> 0 || m land 1 <> 0 then aux (c lsr 1) (m lsr 1) (n+1)
    else aux (c lsr 1) (m lsr 1) n
  in aux c m 0
;;

let merge_word a b = 
  let (n1, m1) = a and (n2, m2) = b in
  if m1 <> m2 then failwith "Pas les mêmes masques"
  else (n1 land m1, (n1 lxor n2) lor m1)
;;

let minterm_to_binword l nb_var =
  let m = ((-1) lsl nb_var) in
  let rec aux l accu = match l with
    | x::r -> aux r ((x,m)::accu) (* -1 = lnot 0 *)
    | _ -> accu
  in aux l []
;;

let print_minterm (x,m) nb_var =
  let checker = 1 lsl (nb_var -1) in
  let rec aux x m n =
    if n > nb_var then ()
    else match (x,m) with
    | 0, 0 -> ()
    | _, _ ->
      if m land checker = 0 then begin 
	print_int ((x land checker) lsr (nb_var-1));
	aux (x lsl 1) (m lsl 1) (n+1)
      end
      else begin print_char '_';
	aux (x lsl 1) (m lsl 1) (n+1)
      end
  in aux x m 1
;;


(*let count l predicat =
  let rec aux n l = match l with
    | x::r -> if (predicat x) then aux (n+1) r else aux n r
    | _ -> n
  in aux l
;;
 *)
