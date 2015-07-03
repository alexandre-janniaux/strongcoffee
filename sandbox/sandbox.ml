open Format;;
open MultipleValued;;
open MVDebug;;
open Expand;;
open Espresso;;
open Tautology;;
open Complement


let _ = 
  let cov_test_compl = List.map cube_from_text (
    "10000101 10011001010 001010101010 101111001" ::
    "10101101 10101011101 101011110110 101011010" ::
    "00110110 10101010101 101101010110 101010110" ::
    "10101101 11111101110 101010111011 010101110" ::
    []) in
  let off_set = sop_complement cov_test_compl in
  print_sop off_set;
  let cover = List.map cube_from_text (
    "10 10 10" ::
    "01 10 10" ::
    "10 01 10" ::
    "10 10 01" ::
    []) in
    print_sop cover;
  print_sop (list_map_n (list_map_n (fun l->l)) cover);
  let dc = [cube_from_text "01 01 10"] in
  let cover' = espresso_mv cover dc {nb_var = 6; var_size = [2;2;2;2;3;5]} in
  print_sop cover'

