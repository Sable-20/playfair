(* 
TODO make the key table function and the searching
*)

(* https://www.geeksforgeeks.org/playfair-cipher-with-examples/ *)
(* : char list list list list list *)
(* 
  let key = [
    [ [] ; [] ; [] ; [] ; [] ]; 
    [ [] ; [] ; [] ; [] ; [] ]; 
    [ [] ; [] ; [] ; [] ; [] ]; 
    [ [] ; [] ; [] ; [] ; [] ]; 
    [ [] ; [] ; [] ; [] ; [] ]; 
    ]
*)

(* 
    match length string =
    | 0 -> if even
    | _ -> if odd
*)

(* steps
  1. take the string
  2. iterate over the string using `iteri`
  3. split the string into groups of two 
  4. pass it to another function to generate the key
  5. use the key to generate an encrypted string 
*)

list1 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"];;


let remove_spaces text =
  let new_text = ref "" in
  String.iter (fun i ->
    if i <> ' ' then
      new_text := !new_text ^ String.make 1 i
    ) text;;
  !new_text


let diagraph text =
  let diagraph = ref [] in 
  let group = ref 0 in
  for i = 2 to String.length text - 1 do
    if i mod 2 = 0 then (
      diagraph := (String.sub text !group (i - !group)) :: !diagraph;;
      group := i;;
    )
  done;;
  diagraph := (String.sub text !group (String.length text - !group)) :: diagraph;;
  List.rev !diagraph

let rec filter_letter text =
  let k = String.length text in
  let rec aux i =
    if i >= k - 1 then text
    else if text.[i] = text.[i + 1] then
      let new_word = String.sub text 0 (i + 1) ^ "x" ^ String.sub text (i + 1) (k - (i + 1)) in
      filler_letter new_word
    else
      aux (i + 2)
  in
  if k mod 2 = 0 then aux 0
  else aux 0

let generate_key word list_to_use =
  let key_letters = [] in
