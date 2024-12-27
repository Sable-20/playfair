let rec split' str pos n =
  if pos >= String.length str then
    []
  else 
    String.sub str pos n :: split' str (pos + n) n
  ;;

let split str n = 
  if (String.length str) mod 2 == 0 then
    split' str 0 n
  else
    split' (str ^ "x") 0 n
  ;;

let rec fill' str pos =
  let len = String.length str in
  if pos >= len - 1 then
    str
  else if str.[pos] == str.[pos + 1] then
    fill' (String.sub str 0 (pos + 1) ^ "x" ^  String.sub str (pos + 1) (len - (pos + 1))) 0
  else
    fill' str (pos + 2)
  ;;

let fill str = 
  fill' str 0
  ;;
