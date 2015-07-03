open OUnit
open MultipleValued
open Espresso
open Partition
open Tautology
open MVDebug
open Complement


(* This function and sop_contains must be tested for validity *)
let assert_cover_is (cov1:sop_t) (cov2:sop_t) =
  let f s x = not @@ sop_contains s x in
  assert_equal false (List.exists (f cov2) cov1);
  assert_equal false (List.exists (f cov1) cov2)
  

let sop_check sop1 sop2 =
  let f s x = not @@ sop_contains s x in
  not (List.exists (f sop2) sop1) && not (List.exists (f sop1) sop2)
 

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
  let s1 = sop_from_text ("10 11" :: "11 01" :: [])
  and s2 = sop_from_text ("01 11" :: [])
  and s3 = sop_from_text ("10 01" :: "01 10" :: []) in
  assert_equal 
    ~msg: "L'intersection est correcte."
    true 
    (sop_check (sop_intersect s1 s2) (sop_from_text ("01 01"::[])));
  assert_equal
    ~msg: "L'intersection ne comprend pas les termes vides."
    false
    (sop_intersect s1 s3 |> List.exists cube_is_empty)
    
let test_literal_supercube () =
  let x1 = [true;false]
  and x2 = [true;true]
  and x3 = [false;true]
  and x4 = [false;false] in
  assert_equal [true;true] (literal_supercube x1 x2);
  assert_equal [true;true] (literal_supercube x1 x3);
  assert_equal [true;false] (literal_supercube x1 x4)
    
let test_cube_supercube () =
  let c1 = cube_from_text "1001 1010 10110"
  and c2 = cube_from_text "1010 1010 11000" 
  and c3 = cube_from_text "1010 1010 00000" 
  and c4 = cube_from_text "1111 1111 11111" in 
  assert_equal 
    ~msg: "L'opération supercube est bien définie."
    (cube_from_text "1011 1010 11110")
    (cube_supercube c1 c2);
  assert_equal
    ~msg: "Le supercube d'un cube et du vide donne le cube."
    c1
    (cube_supercube c1 c3);
  assert_equal
    ~msg: "Le supercube d'un cube et du tout donne le tout."
    c4
    (cube_supercube c1 c4)

let test_literal_contains () =
  let x1 = [false;true;true]
  and x2 = [false;true;false]
  and x3 = [true;true;true] in
  assert_equal true (literal_contains x1 x2);
  assert_equal false (literal_contains x1 x3)


let test_cube_cofactor () =
  let c1 = cube_from_text "010 101"
  and c2 = cube_from_text "011 101" 
  and c3 = cube_from_text "001 101" in 
  assert_equal 
    ~printer: (cube_to_string)
    ~msg: "cofactoring two non disjoint cube works."
    (cube_from_text "110 111") 
    (cube_cofactor c1 c2);
  assert_equal
    ~printer: (cube_to_string)
    ~msg: "cofactoring a cube with a disjoint cube gives an empty cube."
    (cube_from_text "000 000")
    (cube_cofactor c1 c3);
  assert_equal
    ~printer: (cube_to_string)
    ~msg: "cofactoring a cube with itself gives a full cube."
    (cube_from_text "111 111")
    (cube_cofactor c3 c3)

    
let test_is_not_tautology_col () =
  let sop = List.map cube_from_text 
              ( "101 100" ::
                "100 001" :: []) in
  let sop' = List.map cube_from_text
               ( "110 101" ::
                 "101 100" ::
                 "000 011" :: []) in
  assert_equal 
    ~msg: "reconnait une colonne de zéros."
    true 
    (is_not_tautology_col sop);
  assert_equal
    ~msg: "reconnait qu'il n'y a pas de colonne de zéros."
    false
    (is_not_tautology_col sop')

    

let test_cube_distance () =
  let c1 = cube_from_text "111010 100110 100110" 
  and c2 = cube_from_text "101010 100010 011000"
  and c3 = cube_from_text "001000 100000 100000" in
  assert_equal
    ~msg: "cube_distance calcule une distance de 1."
    1
    (cube_distance c1 c2);
  assert_equal
    ~msg: "cube distance calcule une distance de 0."
    0
    (cube_distance c1 c3)

let test_cube_contains () =
  let c1 = cube_from_text "110110 10110 1011001" 
  and c2 = cube_from_text "100000 00110 1000001"
  and c3 = cube_from_text "111110 10110 1011001" in
  assert_equal
    ~msg: "c1 contient c2"
    true
    (cube_contains c1 c2);
  assert_equal
    ~msg: "c1 ne contient pas c3"
    false
    (cube_contains c1 c3)


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


let test_is_tautology_row () = 
  let sop1 = sop_from_text ("11 11 111"::"00 00 000"::[])
  and sop2 = sop_from_text ("11 11 110"::"11 01 111"::[]) in
  assert_equal
    ~msg: "sop1 est une tautologie triviale."
    true
    (is_tautology_row sop1);
  assert_equal
    ~msg: "sop2 n'est pas une tautology triviale"
    false
    (is_tautology_row sop2)
      

let test_vect_or_col () =
  let sop = sop_from_text ("101 101 101"::"111 010 101"::[]) in
  assert_equal
    ~printer: cube_to_string
    ~msg: "vect_or_col fait un or sur les colonnes"
    (cube_from_text "111 111 101")
    (vect_or_col sop)


let test_vect_weakly_unate () =
  let sop = sop_from_text
      ("101 111 101"::
       "101 101 111"::
       "111 101 100"::[]) in
  assert_equal
    ~printer: cube_to_string
    ~msg: "vect_weakly_unate indique en quelle variable et pour quel colonne la fonction est weakly unate."
    (cube_from_text "010 010 010")
    (vect_weakly_unate sop)


let test_is_weakly_unate () =
  let sop1 = sop_from_text
      ("101 111 101"::
       "101 101 111"::
       "111 101 100"::[]) 
  and sop2 = sop_from_text
      ("110 111 101"::
       "101 101 111"::
       "111 101 100"::[]) in
  assert_equal
    ~msg: "Les fonctions weakly unate sont reconnus."
    true
    (is_weakly_unate sop1);
  assert_equal
    ~msg:"Une fonction qui n'est pas weakly unate en chacune de ses variables n'est pas reconnue."
    false
    (is_weakly_unate sop2)


(* TODO: vérifier tous les cas *)
let test_is_tautology () =
  let sop1 = sop_from_text
      (""::[])
  and sop2 = sop_from_text
      (""::[])
  in 
  assert_equal
    ~msg: "Une tautologie est bien reconnue comme telle."
    true
    (is_tautology sop1);
  assert_equal
    ~msg: "Une non-tautologie est bien écartée."
    false
    (is_tautology sop2)




let test_fixture = "Espresso" >:::
                   [
    (* MULTIPLE VALUED *)
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
    "cube_cofactor" >:: test_cube_cofactor;

    (* PARTITION *)
    "partition" >:: test_partition;

    (* TAUTOLOGY *)
    "is_tautology_row" >:: test_is_tautology_row;
    "vect_or_col" >:: test_vect_or_col;
    "is_not_tautology_col" >:: test_is_not_tautology_col;
    "vect_weakly_unate" >:: test_vect_weakly_unate;
    "is_weakly_unate" >:: test_is_weakly_unate;
    "is_tautology" >:: test_is_tautology
  ]

let _ =
  run_test_tt (*~verbose:true*) test_fixture

;;
