open MultipleValued
open Format
open MVDebug

    
(*   solves the partition problem with the 
 *   number of covered variable as as cost function 
 *)
let partition_glouton (l:(int*int)list) : (int list*int list) =
  let (_,l1,_,l2) = List.fold_left (fun (w1, l1, w2, l2) (i,weight) ->
    (*print_string "\nGLOUTON \n";
    print_string "i="; print_int i;
    print_string " | w = "; print_int weight;*)
    if w1 < w2 then (w1+weight, i::l1, w2, l2)
    else (w1, l1, w2+weight, i::l2)
  ) (0, [], 0, []) l
  in (List.rev l1,List.rev l2)
;;

(*
 * Choose the optimal splitting variable 
 * which has the most active values (i.e, 0 parts).
 *)
let choose_splitting_variable (sop:sop_t) : int =
  (* TODO: vérifier s'il s'agit un map ou map_n ici *)
  let var_count_per_cube = list_map_n (List.map (list_count ((=)false))) sop in
  let var_count = List.map list_sum var_count_per_cube in
  let (_, j, _) = 
    List.fold_left (fun (i,j,m) x -> if x > m then (i+1,i,x) else (i+1,j,m)) (0,0,-1) var_count
  in j
;;
   
let partition (sop:sop_t) : cube_t*cube_t =
  match sop with
  | [] -> failwith "vide"
  | x::r ->
    let model = List.map make_universe x in
    let var_index = choose_splitting_variable sop in
    let values =  List.map (fun x -> List.nth x var_index) sop |>
                  list_map_n (list_count ((=)false)) in
    let (c1, c2) = 
      if List.length values > 2 then begin
        let weights = List.combine (range (List.length values)) values in
        let l1, l2 = partition_glouton weights in

        let rec aux l1 l2 c1 c2 = match l1,l2 with
          | x::r1, y::r2 when x < y ->  aux r1 l2 (true::c1) (false::c2)
          | x::r1, y::r2 -> aux l1 r2 (false::c1) (true::c2)
          | [], [] -> List.rev c1, List.rev c2
          | x::r1, [] -> aux r1 [] (true::c1) (false::c2)
          | [], y::r2 -> aux [] r2 (false::c1) (true::c2)
        in
        aux l1 l2 [] [] 
      end
      else if List.length values = 2 then 
        [false;true], [true;false]
      else
        [true], [false]
      in
    let build_var l c = 
      let (_,v) = List.fold_left (fun (i,accu) x ->
	if i <> var_index then (i+1, (x::accu)) else (i+1, c::accu))
	(0,[]) l
      in List.rev v
    in let x1 = build_var model c1 and x2 = build_var model c2
    in 
    (*print_string "partition : \n"; 
    print_cube x1; 
    print_newline ();
    print_cube x2; 
    print_newline ();*)
    (x1,x2)
    

