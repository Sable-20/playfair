let rec split' str pos n =
  if pos >= String.length str then
    []
  else
    String.sub str pos n :: split' str (pos+n) n
  ;;

let _split str n =
  if (String.length str) mod 2 == 0 then
    split' str 0 n
  else
    split' (str ^ String.make 1 'x') 0 n
  ;;
