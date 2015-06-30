open MultipleValued
open Str
open Format



let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let print_cube (cube:cube_t) = 
  List.iter (fun var -> 
    List.iter (fun x ->
      if x then print_int 1 else print_int 0
    ) var;
    print_string " "
  ) cube

let print_sop (sop:sop_t) = 
  print_string "== print SOP ==";
  print_newline ();
  List.iter (fun cube -> 
    print_cube cube;
    print_newline ())
    sop;
  print_string "== stop SOP==";
  print_newline ()

let cube_from_text txt = 
  let vars = Str.split (Str.regexp " ") txt in
  List.map (fun t ->
    List.map (fun x -> if x='1' then true else false) t) 
    (List.map explode vars)


