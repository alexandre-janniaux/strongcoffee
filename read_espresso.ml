
#load "str.cma";;

let read_file filename = 
  try
    let f = open_in filename in
    let rec read accu = 
      try
	let l = input_line f in
	read (l::accu)
	
      with End_of_file -> List.rev accu
    in

    let rec aux lines (nb_in,nb_out,inputs) =
      match lines with
      | line::r -> begin
	if line = "" then (nb_in, nb_out,inputs)
	else if line.[0] = '.' then begin
	  if line.[1] = 'i' && nb_in = 0 then
	    let nb_in = int_of_string (List.hd (Str.split (Str.regexp ".i ") line)) in
	    aux r (nb_in, nb_out,inputs)
	  else if line.[1] = 'o' && nb_out = 0 then
	    let nb_out = int_of_string (List.hd (Str.split (Str.regexp ".o ") line)) in
	    aux r (nb_in, nb_out, inputs)
	  else failwith "malforme"
	end else
	  if nb_in = 0 || nb_out = 0 then failwith "pas de nb entre/sorti defini"
	  else
	    match (Str.split (Str.regexp " ") line) with
	    | (i:string)::(o:string)::[] when (i<>"" && o<>"") -> aux r (nb_in, nb_out, (i,o)::inputs)
	    | _ -> failwith "erreur"
      end
      | _ -> (nb_in, nb_out, inputs)
    in let lines = read [] in if lines = [] then failwith "vide" else
       close_in f; aux lines (0,0,[])
  with _ -> print_string "not found";(0,0,[])
;;
