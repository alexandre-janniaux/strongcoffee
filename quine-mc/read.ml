
open Str

let read_file f_parse filename = 
  try
    let f = open_in filename in
    let rec read accu = 
      try
        let l = input_line f in
        read (l::accu)

      with End_of_file -> List.rev accu
    in

    let rec aux lines inputs =
      match lines with
      | line::r ->
        if line.[0] = '#' then aux r inputs
        else aux r (f_parse line::inputs)
      | [] -> inputs
    in 
    let lines = read [] in 
    if lines = [] then failwith "vide" 
    else close_in f; aux lines []
  with _ -> print_string "not found";[]
;;
