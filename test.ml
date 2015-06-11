#use "binword.ml";;
#use "read_espresso.ml";;
#use "quine3.ml";;
#use "coverproblem.ml";;

let is_in_cover x cover =
  let rec aux cover = match cover with
    | y::r -> if is_cover y x then true else aux r
    | _ -> false
  in aux cover
;;

let check_cover cover on_cover =
  let rec aux on_cover = match on_cover with
    | x::r -> if is_in_cover x cover then true else aux r 
    | _ -> false
  in aux on_cover
;;

let cost cover =
  let rec aux cover = List.fold_left
    (fun a l -> a + List.fold_left(
      fun a x-> match x with
      | Wildcare -> a
      | _ -> a+1
     ) 0 l
    ) 0 cover
  in aux cover
;;

let main () = 
  let nb_in, nb_out, test_circuit = read_file "test_circuit.txt" in
  let test = List.map (fun (x,y) -> bin_to_binword (string_to_list x)) (*(List.filter (fun (x,y) -> y.[0] = '1') test_circuit)*) test_circuit in
  let implicants = quine test in
  let reduc = compute_mincover implicants test in
  nb_in, cost test, cost reduc, check_cover reduc test
;;

