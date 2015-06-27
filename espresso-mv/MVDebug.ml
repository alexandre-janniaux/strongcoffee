open MultipleValued
open Format

let print_cube cube = 
  List.iter (fun var -> 
    List.iter (fun x ->
      if x then print_int 1 else print_int 0
    ) var;
    print_string "-"
  ) cube
