let list_compar l1 l2 =
  let list1 = List.sort (fun x y -> if x>y then 1 else 0 ) l1 in
  let list2 = List.sort (fun x y -> if x>y then 1 else 0 ) l2 in
  let rec traitment l1 l2 =
    match (l1,l2) with
    | (elem1::rest1,elem2::rest2) -> if elem1 == elem2 then traitment rest1 rest2 else change := true
    | ([],[]) -> ()
    | ([],_) -> change := true
    | (_,[]) -> change := true
  in traitment list1 list2;
  ()
in
