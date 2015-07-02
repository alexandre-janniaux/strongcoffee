open MultipleValued
open Str
open Format



let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let cube_to_string cube = 
  let str = ref "[" in 
  List.iter (fun var -> 
    List.iter (fun x ->
      str := !str ^ (if x then  "1" else "0")
    ) var;
    str := !str ^ " "
  ) cube;
  (!str) ^ "]"


let print_cube (cube:cube_t) = 
  print_string @@ cube_to_string cube


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

let sop_from_text list_txt = 
  List.map cube_from_text list_txt

let debug location txt = 
  print_string ("[DEBUG:" ^ location ^ "] : " ^ txt);
  print_newline ();

