open Printf;;

type  variable =
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

let content_string = "[\"fejwaofjweoai,jfioewa\",\"fjewofew\",\"fejwaojfoiewa\", fewaoifjeowajfo, jfewa]";;
let content_string = "\"fejwaofjoewa fewa\""
let test_string = "\"hgoefewjaofjeowiafoewadkajf\"  feawfewa";;

let rec extract_string string idx flag=
  let nth_char = String.get string idx in
  if nth_char = '"' && flag = false then extract_string string (idx+1) true
  else if nth_char = '"' && flag = true then idx
  else extract_string string (idx+1) flag
;;

let rec print_hoge lists idx =
  try
    let nth_elm = List.nth lists idx in 
    let _ = match nth_elm with
    | Variable(x) -> print_int idx; Printf.printf "variable_test = %s \n" x;
    | Lists (list_) -> print_string "List_variable_test \n"; print_hoge list_ 0;
    in
    print_hoge lists (idx+1);
  with e->()
;;

let lists_add list var =
  
  let new_list = match list with 
   | Lists (list_) ->  Lists (list_ @ [Variable var])
  in 
  new_list
;;

let detect_string_type_or_other_and_return_nwe_variables _string _variables _dict = 
  if String.length _string >= 2 && String.get _string 0 = '"' && String.get _string ((String.length _string)-1) = '"' then
    let _ = print_string "matched string\n" in
    let last_elm = List.nth _variables ((List.length _variables)-1) in
    let removed_last_elm = List.filter (fun x -> x != last_elm) _variables in 
    let new_variables = removed_last_elm @ [lists_add last_elm _string] in
    new_variables
  else 
    let _ = print_string "matched dict\n" in
    let last_elm = List.nth _variables ((List.length _variables)-1) in
    let removed_last_elm = List.filter (fun x -> x != last_elm) _variables in 
    let new_variables = removed_last_elm @ [lists_add last_elm _string] in 
    new_variables
;;

(*
print_hoge [Lists [Variable "fejwaofewa"; Variable "fejowafjoewa"]] 0;;
lists_add (Lists [Variable "fejwaofewa"; Variable "fejowafjoewa"]) "hoge";;

print_string "--------------------------------\n";;
*)

let rec hoge bit_string args variables char_idx array_flag = 
  if char_idx >= String.length content_string then 
    let content = delete_white_space bit_string in
    if String.length content > 0 && String.get content 0 <> '"' && String.get content ((String.length content)-1) <> '"' then 
      let new_variables = variables @ [Variable content] in
      let new_args = args @ new_variables in
      char_idx, new_args
    else 
      let new_args = args @ variables in
      char_idx, new_args
  else 
  let idx_char = String.get content_string char_idx in 
  let new_string =  String.concat "" [bit_string; String.make 1 idx_char] in
  (* toji kakko de idx to data wokaesu *) 
  if idx_char = '[' then
    hoge "" args (variables@[Lists []]) (char_idx+1)true
  else if idx_char = ']' then
    let new_variables = detect_string_type_or_other_and_return_nwe_variables bit_string variables () in
    hoge "" args new_variables (char_idx+1) array_flag
  else if idx_char = '(' then
    if bit_string = "split" then
      let (_char_idx, args) = hoge "" [] [] (char_idx+1) false in
      hoge "" [] [] (_char_idx+1) false
    else (* if hoge_string = "other" then*)
      let (_char_idx, args) = hoge "" [] [] (char_idx+1) false in
      hoge "" [] [] (_char_idx+1) false 
  else if idx_char = ')' then
    let new_args = args @ variables in
    char_idx, new_args
  else if idx_char = ',' then
    if array_flag = true then
      let new_variables = detect_string_type_or_other_and_return_nwe_variables bit_string variables () in
      hoge "" args new_variables (char_idx+1) array_flag
    else
      let new_args = args @ variables in
      hoge "" new_args [] (char_idx+1) array_flag
  else if idx_char = '"' then 
    let end_quote_idx =  extract_string content_string char_idx false in
    let new_bit_string = String.sub content_string (char_idx) (end_quote_idx-char_idx+1)in  
    hoge new_bit_string args variables (end_quote_idx+1)  array_flag
  else if idx_char = ' ' then
    hoge bit_string args variables (char_idx+1) array_flag
  else
    hoge new_string args variables (char_idx+1) array_flag
;;

let (char_idx, args) = hoge "" [] [] 0  false;;
let rec print lists idx = 
  let nth_elm = List.nth lists idx in 
  let _ = match nth_elm with
  | Variable(x) -> Printf.printf "variable = %s \n" x
  | Lists (list) -> print_string "List_variable \n"; print list 0
  in
  print lists (idx+1);
;;
try
  print args 0
with e->
  ()
;

 
