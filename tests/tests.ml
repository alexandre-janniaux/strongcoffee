open OUnit
open MultipleValued
open Espresso
open Partition
open Tautology
open MVDebug


(* This function and sop_contains must be tested for validity *)
let assert_cover_is (cov1:sop_t) (cov2:sop_t) =
  let f s x = not @@ sop_contains s x in
  assert_equal false (List.exists (f cov2) cov1);
  assert_equal false (List.exists (f cov1) cov2)
  




let l1 = [1;2;3;4] and l2 = [3;4;5;6] and l_count = [1;1;2;1;2;1]

let test_list_union () =
  assert_equal [1;2;3;4;5;6] (list_union l1 l2);
  assert_equal l1 (list_union l1 l1)

let test_list_count () =
  assert_equal 4 (list_count ((=)1) l_count);
  assert_equal 2 (list_count ((<>)1) l_count)

let test_list_sum () =
  assert_equal 45 (list_sum [1;2;3;4;5;6;7;8;9])

let test_list_map_n () =
  let l = [[1;2;3;4];[2;3;4;5];[3;4;5;6];[7;8;9;10]] in
  assert_equal [[1;2;3;7];[2;3;4;8];[3;4;5;9];[4;5;6;10]] (list_map_n (fun x->x) l)

let test_literal_is_empty () =
  let x1 = [false; true] and x2 = [false; false] in
  assert_equal false (literal_is_empty x1);
  assert_equal true (literal_is_empty x2)

let test_cube_is_empty () =
  let c1 = [[false;true];[true;false];[true;true]]
  and c2 = [[false;true];[true;false];[false;false]] in
  assert_equal false (cube_is_empty c1);
  assert_equal true (cube_is_empty c2)

let test_cube_is_universal () =
  let c1 = [[false;true];[true;true];[true;true;true]]
  and c2 = [[true;true];[true;true];[true;true;true]] in
  assert_equal false (cube_is_universal c1);
  assert_equal true (cube_is_universal c2)

let test_sop_is_universal () =
  let sop1 = [ [[false;true];[true;false]]; [[false;false];[false;true]] ]
  and sop2 = [ [[false;true];[true;false]]; [[true;true];[true;true]] ] in
  assert_equal false (sop_is_universal sop1);
  assert_equal true (sop_is_universal sop2)

let test_literal_complement () =
  let literal = [false;true;false] in
  assert_equal [true;false;true] (literal_complement literal)

let test_cube_complement () =
  let c1 = [[false;true];[true;false];[false;false]]
  and c2 = [[false;true];[true;false];[false;true]] in
  assert_equal true (cube_complement c1 |> sop_is_universal);
  assert_equal false (cube_complement c2 |> sop_is_universal)
    
let test_literal_intersect () =
  let x1 = [true;false;true]
  and x2 = [false;false;true] in
  assert_equal [false;false;true] (literal_intersect x1 x2);
  assert_equal (literal_intersect x1 x2) (literal_intersect x2 x1);
  assert_equal x1 (literal_intersect x1 x1)

let test_cube_intersect () =
  let c1 = [[true;false];[false;true]]
  and c2 = [[true;true];[false;false]] in
  assert_equal [[true;false];[false;false]] (cube_intersect c1 c2);
  assert_equal (cube_intersect c1 c2) (cube_intersect c2 c1)

let test_sop_intersect () =
  let s1 = [ [[false;true];[true;true]]; [[true;true];[false;true]] ]
  and s2 = [ [[false;true];[true;true]] ]
  and s3 = [ [[true;false];[false;true]]; [[false;true];[true;false]] ] in
  assert_equal [] (sop_intersect s1 s2);
  assert_equal [] (sop_intersect s1 s3)
    
let test_literal_supercube () =
  let x1 = [true;false]
  and x2 = [true;true]
  and x3 = [false;true]
  and x4 = [false;false] in
  assert_equal [true;true] (literal_supercube x1 x2);
  assert_equal [true;true] (literal_supercube x1 x3);
  assert_equal [true;false] (literal_supercube x1 x4)
    
let test_cube_supercube () =
  assert_equal false true

let test_literal_contains () =
  let x1 = [false;true;true]
  and x2 = [false;true;false]
  and x3 = [true;true;true] in
  assert_equal true (literal_contains x1 x2);
  assert_equal false (literal_contains x1 x3)


let test_cube_cofactor () =
  let c1 = [ [false;true;false]; [true;false;true] ]
  and c2 = [ [false;false;true]; [true;false;true] ] in
  assert_equal [] (cube_cofactor c1 c2)
    
    
    
let test_cube_contains () =
  let c1 = []
  and c2 = [] in
  assert_equal true (cube_contains c1 c2)
    
let test_is_not_tautology_col () =
  let sop =
    List.map cube_from_text 
      [
        "101 100";
        "100 001";
      ]
  in
  assert_equal true (is_not_tautology_col sop)
    

let test_cube_distance () =
  todo "make a cube distance test"

let test_cube_contains () =
  todo ""
    

let test_sop_contains () =
  todo "make a sop_contains test"


let test_cube_sharp () =
  todo "make a cube_sharp test"

let test_essential () =
  let on_set = [
    [ []; []; [] ];
    [ []; []; [] ];
    [ []; []; [] ];
    [ []; []; [] ]
  ] in
  let essentials = [
  ] in
  assert_cover_is essentials (sop_essentials on_set [])

let test_partition () = 
  let sop = [ 
    [[false;true;false]; [true;false;false]]; 
    [[false;false;true]; [false;true;false]] ] 
  in
  let c1,c2 = partition sop in
  assert_equal 
    ~msg:"L'intersection deux à deux des éléments d'une partition est vide"
    true 
    (cube_intersect c1 c2 |> cube_is_empty);
  assert_equal 
    ~msg:"L'union des élements d'une partition est une tautologie"
    true 
    (is_tautology [c1;c2]) (* TODO : other check (taugology use partition) *)
      
    
let test_fixture = "Espresso" >:::
  [
    (*"list_union" >:: test_list_union;*)
    "list_count" >:: test_list_count;
    "list_sum" >:: test_list_sum;
    "list_map_n" >:: test_list_map_n;
    "literal_complement" >:: test_literal_complement;
    "cube_complement" >:: test_cube_complement;
    "literal_is_empty" >:: test_literal_is_empty;
    "cube_is_empty" >:: test_cube_is_empty;
    "cube_is_universal" >:: test_cube_is_universal;
    "literal_intersect" >:: test_literal_intersect;
    "cube_intersect" >:: test_cube_intersect;
    "sop_intersect" >:: test_sop_intersect;
    "literal_supercube" >:: test_literal_supercube;
    "cube_supercube" >:: test_cube_supercube;
    "literal_contains" >:: test_literal_contains;
    "cube_contains" >:: test_cube_contains;
    "sop_contains" >:: test_sop_contains;
    "cube_distance" >:: test_cube_distance;
    "cube_sharp" >:: test_cube_sharp;


    "is_not_tautology_col" >:: test_is_not_tautology_col;
    "partition" >:: test_partition
  ]

let _ =
  run_test_tt (*~verbose:true*) test_fixture

;;
