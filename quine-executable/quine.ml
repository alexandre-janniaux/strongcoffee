open Read
open Tools
open Expand
open Covering

let _ = 
  assert (Array.length Sys.argv =2);
  print_string ("Chargement du fichier: " ^ Sys.argv.(1) ^ "\n");
  let sop = read_file cube_from_bitstr Sys.argv.(1) in
  let sop' = expand sop in
  let irr_sop = irredundant sop sop' in
  let sop_exact = petrick_covering sop sop' in

  print_string "\n[RESULTAT]:\n";
  print_sop irr_sop;
  print_newline();
  if sop_verify sop irr_sop then
    print_string "La couverture trouvée est bien valide."
  else 
    print_string "Il y a une erreur : la couverture n'est pas complète.";
  print_newline();

  print_string "\n[STATS]: initial \n";
  print_string "Cubes : "; print_int (List.length sop); print_newline();
  print_string "Littéraux : "; print_int (sop_cost sop); print_newline();
  
  print_string "\n[STATS]: heuristique \n";
  print_string "Cubes : "; print_int (List.length irr_sop); print_newline();
  print_string "Littéraux : "; print_int (sop_cost irr_sop); print_newline();

  print_string "\n[STATS]: exact \n";
  print_string "Cube : "; print_int (List.length sop_exact); print_newline();
  print_string "Littéraux : "; print_int (sop_cost sop_exact); print_newline();


