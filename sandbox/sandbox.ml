open Format;;
open MultipleValued;;
open MVDebug;;
open Expand;;
open Espresso;;
open Tautology;;
open Complement


let _ = 
  let cover = List.map cube_from_text (
    "11 10 10" ::
    "01 01 11" ::
    [])
  in
  let cover' = espresso_mv cover [] {nb_var = 6; var_size = [2;2;2;2;3;5]} in
  print_sop cover'

