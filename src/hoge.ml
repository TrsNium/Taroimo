open Printf;;

type  variable =
  | Variable of string
  | Lists of variable list;;
  
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;
let delete_white_space l = String.map (fun x -> if x = ' ' then '\x00' else x) l;;

let content_string_ = "[\"fjoewajoifewa\", \"fjeoawfjoie\"]";;
let content_string_ = "[\"fjoewajoifewa\", fjeoawfjoie]";;
let content_string_ = "\"fejwaofjoewa fewa\"";;
let content_string = "join(\",\", [fewa, fewa])";;

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

(* print test
print_hoge [Lists [Variable "fejwaofewa"; Variable "fejowafjoewa"]] 0;;
lists_add (Lists [Variable "fejwaofewa"; Variable "fejowafjoewa"]) "hoge";;

print_string "--------------------------------\n";;
*)

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

let detect_quote_string_type_or_other_and_return_new_variables _string _variables _dict = 
  (* matched other type, reference from _dict *)
  if String.length _string > 0 && String.get _string 0 <> '"' && String.get _string ((String.length _string)-1) <> '"' then 
    (* TODO: add  reference from dict *)
    _variables @ [Variable _string]
  else if String.length _string > 2 then(* matched string_type *)
    let string_length = if String.length _string -2 <= 0 then 1 else String.length _string -2 in
    let removed_quote_string = String.sub _string 1 string_length in
    _variables @ [Variable removed_quote_string]
  else 
    _variables
;;

let rec variable_lists_to_list variable_list lists idx =
  if List.length variable_list = idx then
    lists
  else
    let nth_elm = List.nth variable_list idx in
    let _nth_elm = match nth_elm with
      |Variable (_nth_elm) -> _nth_elm
    in 
    variable_lists_to_list variable_list (lists@[_nth_elm]) (idx+1)
;;

(* test variable_lists_to_list 
let rec _test_variable_lists_to_list string_list idx= 
   let nth_elm = List.nth string_list idx in
   Printf.printf "test_variable_lists_to_list %s\n" nth_elm ;
   _test_variable_lists_to_list string_list (idx+1)
;;

let _test_variable_lists = [Variable "fewafewa"; Variable "fejwaofjeowa"];;
let _test_variable_lists2list = variable_lists_to_list _test_variable_lists [] 0 ;;
try
  _test_variable_lists_to_list _test_variable_lists2list 0
with e ->
  0
;; *)

let evalute_function _method_name _args = 
  if _method_name = "join" then
    let results = match _args with 
    | [Variable _sep; Lists extracted_list] ->
      let string_list =  variable_lists_to_list extracted_list [] 0 in
      String.concat _sep string_list
    in
    Variable results
  else 
    Variable ""
;;

let rec hoge bit_string args variables char_idx array_flag = 
  if char_idx >= String.length content_string then 
    let new_variables = detect_quote_string_type_or_other_and_return_new_variables bit_string variables () in
    char_idx, (args @ new_variables)
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
    (* bit_string means method_name*)  
    let (_char_idx, _args) = hoge "" [] [] (char_idx+1) false in
    let results = evalute_function bit_string _args in 
    hoge "" (args@[results]) [] (_char_idx+1) false
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
    if array_flag = true then
      hoge new_bit_string args variables (end_quote_idx+1)  array_flag
    else
      let new_variables = detect_quote_string_type_or_other_and_return_new_variables new_bit_string variables () in 
      hoge "" args new_variables (end_quote_idx+1)  array_flag
  else if idx_char = ' ' || idx_char = '\x00' then
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

print_string "-------------------test-----------------\n";;
try
  print args 0
with e->
  ()
