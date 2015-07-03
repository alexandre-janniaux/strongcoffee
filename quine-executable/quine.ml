open Read
open Tools
open Expand
open Covering
open Benchmark

let _ = 
  assert (Array.length Sys.argv >= 2);
  print_string ("Chargement du fichier: " ^ Sys.argv.(1) ^ "\n");
  
  let use_exact = 
    if Array.length Sys.argv = 2 then false else 
    if Sys.argv.(2) = "exact" || Sys.argv.(2) = "both" then true else false in

  let use_backtracking = 
    if Array.length Sys.argv <= 3 then true else
    if Sys.argv.(3) = "backtrack" then true else false in

  let use_heuristic =
    if Array.length Sys.argv = 2 then true else 
    if Sys.argv.(2) = "fast" || Sys.argv.(2) = "both" then true else not use_exact in

  let sop = read_file cube_from_bitstr Sys.argv.(1) in
  let sop' = expand sop in
  (*print_string "\nON_SET\n";
  print_sop sop;
  print_string "\nEXPAND\n";
  print_sop sop';
  print_newline();*)

  print_string "\n[RESULTAT]:\n";
  print_newline();

  let check_result set = 
    if sop_verify sop set then
      print_string "La couverture trouvée est bien valide."
    else 
      print_string "Il y a une erreur : la couverture n'est pas complète.";
    print_newline();
  in


  print_string "\n[STATS]: initial \n";
  print_string "Cubes : "; print_int (List.length sop); print_newline();
  print_string "Littéraux : "; print_int (sop_cost sop); print_newline();

  if use_heuristic then begin
    print_string "\n[STATS]: heuristique \n";
    let heuristic_sop = irredundant sop sop' in
    print_string "Cubes : "; print_int (List.length heuristic_sop); print_newline();
    print_string "Littéraux : "; print_int (sop_cost heuristic_sop); print_newline();
    check_result heuristic_sop
  end;

  if use_exact then begin
    print_string "\n[STATS]: exact \n";
    let exact_sop = petrick_covering sop sop' use_backtracking in
    print_string "Cube : "; print_int (List.length exact_sop); print_newline();
    print_string "Littéraux : "; print_int (sop_cost exact_sop); print_newline();
    check_result exact_sop
  end;

  print_string "\n[BENCHMARK]\n";
  dump_record "expand";
  dump_record "irredundant";
  dump_record "petrick_covering";


