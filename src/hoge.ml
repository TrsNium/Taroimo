let test_string = "split|",", [\"a\"; \"b\"]|";;


type  variable = 
  | Nothing
  | Variable of string
  | Lists of variable list;;
  
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;
let delete_white_space l = String.map (fun x -> if x = ' ' then '\x00' else x) l;;
let extract_content content =
  let _length = String.length content in
  let rec get_quote_idxs current_idx idx_lists = 
    if current_idx = _length then idx_lists
    else
      let c_char = String.get content current_idx in
      let new_idx_lists = idx_lists @ [current_idx] in 
      if c_char = '"' then get_quote_idxs (current_idx+1) new_idx_lists
      else get_quote_idxs (current_idx+1) new_idx_lists
  in
  let quote_idxs =  get_quote_idxs 0 [] in
  let _start = List.nth quote_idxs 0 in
  let _end = List.nth quote_idxs (List.length quote_idxs)-1 in 
  String.sub content (_start + 1)  (_end-_start-1) 
;;

let content_string = "fejwaofjweoaijfioewa";;
let rec hoge bit_string args variables char_idx flag = 
  let idx_char = String.get content_string char_idx in 
  let new_string =  String.concat "" [bit_string; String.make 1 idx_char] in
  (* toji kakko de idx to data wokaesu *) 
  if idx_char = '[' then
    hoge "" [] variables (char_idx+1) true
  else if idx_char = ']' then
    hoge "" [] [] (char_idx+1) false
  else if idx_char = '(' then
    if bit_string = "split" then
      let (_char_idx, structs) = hoge "" [] [] (char_idx+1) false in
      hoge "" [] [] (_char_idx+1) false
    else (* if hoge_string = "other" then*)
      let (char_idx, structs) = hoge "" [] [] (char_idx+1) false in
      hoge "" [] [] (char_idx+1) false 
  else if idx_char = ')' then
    char_idx, args
  else if idx_char = ',' then
    if flag = true then
      let content = delete_white_space bit_string in
      if String.length content >= 2 && String.get content 0 = '"' && String.get content ((String.length content)-1) = '"' then 
        let nth_elm = extract_content bit_string in
        let new_variables = variables @ [Variable nth_elm] in
        hoge "" args new_variables (char_idx+1) true
      else 
        let new_variables = variables @ [Variable content] in
        hoge "" args new_variables (char_idx+1) true
    else
      let new_args = args @ variables in
      hoge "" new_args [] (char_idx+1) false
  else
    hoge new_string args variables (char_idx+1) flag
;;

 
