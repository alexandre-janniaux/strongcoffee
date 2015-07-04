open Format
open Read
open Tools
open Expand
open Covering
open Benchmark


type covering_mode = 
  | Glouton
  | Exact
  | Backtracking
  | OnlyExpand


let solver_mode = ref Glouton

let use_file = ref ""



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
    print_int (sop_cost sop);
    print_string "  ";print_int (sop_cost final_sop);print_newline();
    exec r
  | [] -> ()


let set_solver (solver:string) = match solver with
  | "glouton" -> solver_mode := Glouton
  | "exact" -> solver_mode := Exact
  | "backtrack" -> solver_mode := Backtracking
  | _ -> print_string "Solveur non reconnu. Expansion seulement.\n"; solver_mode := OnlyExpand


let speclist = [
  ("-solver", Arg.String set_solver,
   "Use the heuristic mincov solver");

  ("-file", Arg.String (fun file -> use_file := file),
   "Use a file as input")
]


let usage_message = ""

let _ = 
  Arg.parse speclist print_string usage_message;
  if !use_file <> "" then single !use_file
  else
    exec [
      "nonprime/3.txt";
      "nonprime/4.txt";
      "nonprime/5.txt";
      "nonprime/6.txt";
      "nonprime/7.txt";
      "nonprime/8.txt";
      "nonprime/9.txt";
      "nonprime/10.txt";
      "nonprime/11.txt";
      "nonprime/12.txt";
      "nonprime/13.txt";
      "nonprime/14.txt"
    ]
