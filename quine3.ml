(*#use "binword.ml";;
*)
let quine accepted_words =

  let rec merge w words merged used =
    match words with
    | x::r ->
      if hamming_distance w x = 1 then
	merge w r ((merge_word w x)::merged) (x::used)
      else
	merge w r merged used
    | _ -> merged, used
  in

  let rec step words accu matched prime = match words with
    | x::r ->
      let (merged, used) = merge x r [] [] in
      if merged <> [] then 
	step r (merged@accu) (used@matched) prime
      else
	step r accu matched (x::prime)
    | _ -> accu, matched, prime
  in

  let rec aux words accu =
    if words <> [] then
      let new_words, matched, prime = step words [] [] [] in
      (* supprimer les doublons avant ? *)
      aux new_words (prime@accu)
    else
      accu

  in aux accepted_words []
;;

  
