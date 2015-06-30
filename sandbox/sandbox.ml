open Format;;
open MultipleValued;;
open MVDebug;;
open Expand;;
open Espresso;;
open Tautology;;


let _ = 
  let cover = List.map cube_from_text (
    "11 10 11" ::
    "01 10 10" ::
    "10 01 10" ::
    (*"11 01 01" ::
    "01 01 11" ::
    "01 11 11" ::*)
    [])
  in
  print_sop cover;
  print_cube (vect_or_col cover);
  print_newline();
  print_cube (vect_weakly_unate cover);
  print_newline ();
  let off_cover = sop_complement cover in
  print_string "off_set : ";
  print_newline();
  print_sop off_cover;
  print_string "intersect : \n";
  let inter = sop_intersect cover off_cover in
  print_sop inter;
  if is_tautology (off_cover@cover) then print_string "tautology" else print_string "nontoto"


(*;
  let is_tauto = if is_tautology cover then "est une tautologie" else "n'est pas une tautologie" in
  let cube = cube_from_text "10 01 10" in
  let c = cube_from_text "11 10 01" in
  print_newline();
  print_cube cube;
  print_newline();
  print_cube c;
  print_newline();
  print_cube (cube_intersect cube c);
  print_newline();
  print_string "Distance : "; 
  print_int (cube_distance c cube);
  print_newline();
  print_cube (cube_cofactor cube c);
  let c = cube_from_text "01 11 11" in 
  print_string "\ncofactoring\n" ;
  print_cube (cube_cofactor cube c);
  print_newline();
  print_string is_tauto;
  print_newline ();
  print_sop cover; 
  let off_set = sop_complement cover in
  print_newline ();
  print_string "off_set : \n";
  print_sop off_set;

  let expanded = sop_expand cover off_set in
  print_sop expanded

*)
