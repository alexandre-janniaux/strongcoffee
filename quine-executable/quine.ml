open Format
open Read
open Tools
open Expand
open Covering
open Benchmark
open Str


type covering_mode = 
  | Glouton
  | Exact
  | Backtracking
  | OnlyExpand


let solver_mode = ref Glouton

let use_file = ref ""

let use_dir_regex = ref ""

let use_dir_from = ref 0

let use_dir_to = ref 0


let check_result sop set = 
  if sop_verify sop set then
    print_string "La couverture trouvée est bien valide."
  else 
    print_string "Il y a une erreur : la couverture n'est pas complète.";
  print_newline()


let single filename = 
  let sop = read_file cube_from_bitstr filename in
  let sop' = expand sop in
  print_string "\n====\nON_SET\n====\n";
  print_sop sop;
  print_string "\n====\nEXPAND\n====\n";
  print_sop sop';
  print_newline();

  print_string "\n[RESULTAT]:\n";
  print_newline();

  print_string "\n[STATS]: initial \n";
  print_string "Cubes : "; print_int (sop_cost sop); print_newline();
  print_string "Littéraux : "; print_int (sop_cost sop); print_newline();

  let final_sop =
    match !solver_mode with
    | Glouton ->  
      print_string "\n[STATS]: Glouton \n";
      irredundant sop sop'
    | Exact ->
      print_string "\n[STATS]: Exact naif \n";
      petrick_covering sop sop' false
    | Backtracking ->
      print_string "\n[STATS]: Exact backtracking \n";
      petrick_covering sop sop' true
    | OnlyExpand ->
      print_string "\n[STATS]: Expandsion \n";
      sop'
  in
  print_string "Cubes : "; print_int (List.length final_sop); print_newline();
  print_string "Littéraux : "; print_int (sop_cost final_sop); print_newline();
  check_result sop final_sop


let rec exec liste_f = 
  match liste_f with
  | filename::r -> 
    
    let sop = read_file cube_from_bitstr filename in
    let sop' = expand sop in

    let final_sop = match !solver_mode with
      | Glouton -> irredundant sop sop' 
      | Exact -> petrick_covering sop sop' false
      | Backtracking -> petrick_covering sop sop' true
      | OnlyExpand -> sop'
    in
    if not(sop_verify sop sop') then failwith "erreur : la couverture est invalide" else
    print_int (sop_cost sop);
    print_string "  ";print_int (sop_cost final_sop);print_newline();
    exec r
  | [] -> ()


let set_solver (solver:string) = match solver with
  | "glouton" -> solver_mode := Glouton
  | "exact" -> solver_mode := Exact
  | "backtrack" -> solver_mode := Backtracking
  | _ -> (*print_string "Solveur non reconnu. Expansion seulement.\n"; *) solver_mode := OnlyExpand


let speclist = [
  ("-solver", Arg.String set_solver,
   "Use the heuristic mincov solver");

  ("-input", Arg.String (fun file -> use_file := file),
   "Use a file as input");

  ("-files", Arg.String (fun reg -> use_dir_regex := reg),
   "Use a filename template, see -from and -to");

  ("-from", Arg.Int (fun from -> use_dir_from := from),
   "");

  ("-to", Arg.Int (fun value -> use_dir_to := value),
   "")
]


let usage_message = ""


let build_file_list () = 
  let rec aux i accu = 
    if i < !use_dir_to then 
      aux (i+1) (Str.replace_first (Str.regexp "%i") (string_of_int i) (!use_dir_regex) :: accu)
    else List.rev accu
  in aux !use_dir_from []

let _ = 
  Arg.parse speclist print_string usage_message;

  (*if !use_dir_regex <> "" then*)
  let file_list = build_file_list() in
    exec file_list
  (*else single !use_file *)
