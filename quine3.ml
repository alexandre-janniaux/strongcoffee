#use "binword.ml";;

let quine accepted_words =

  let rec merge w words merged used =
    match words with
    | x::r ->
      if hamming_distance w x (<>) = 1 then
	merge w r ((merge_word w x)::merged) (x::used)
      else
	merge w r merged used
    | _ -> merged, used
  in

  let rec step words accu matched unmatched = match words with
    | x::r ->
      let (merged, used) = merge x words [] [] in
      if merged <> [] then 
	step r (merged@accu) (used@matched) unmatched
      else
	step r accu matched (x::unmatched)
    | _ -> accu, matched, unmatched
  in

  let rec aux words accu =
    let new_words, matched, unmatched = step words [] [] [] in
    if matched <> [] then
      aux new_words (unmatched@accu)
    else
      accu

  in aux accepted_words []
;;

  
