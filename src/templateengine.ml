#use "evaluterutil.ml";;
open Evaluterutil;;

module Templateengine = struct 
type block = { _start : int; _content : string; _end : int};;
let search_literals document = 
  let document_length: int = String.length document in 
  let rec find_out_duplicate_char char start flag = 
    let nth_char = String.get document start in
    (*      debug print
    Printf.printf "%i%c"  start nth_char;
    *)
    if flag == true && nth_char == char then (start - 1)
    else if flag == false && nth_char == char then find_out_duplicate_char char (start+1) true
    else find_out_duplicate_char char (start+1) false
  in

  let rec find_out_block literals start = 
    try 
      let start_idx = find_out_duplicate_char '{' start false  in
      let end_idx = find_out_duplicate_char '}' (start_idx+2) false  in
      let content = String.sub document (start_idx+2) (end_idx - start_idx - 2) in
      
      (*      debug print *)
      Printf.printf "\n start=%i end=%i content = '%s' \n" start_idx end_idx content; 
      
      let element = { _start=start_idx; _content=content; _end=end_idx} in 
      let new_literals = literals @ [element] in
      find_out_block new_literals (end_idx+2);
    with 
      e -> literals;
  in
  find_out_block [] 0;;

let rec proc_Lists result_list original_list idx =
  if idx = List.length original_list then result_list
  else 
    let nth_elm = List.nth original_list idx in
    let extracted_value = match nth_elm with 
    | Evaluterutil.Variable(x) -> x
    in
    proc_Lists (result_list @ [extracted_value]) original_list (idx+1) 
;;

let rec variable_list_to_list reuslt_list original_list idx = 
  if idx = List.length original_list then reuslt_list 
  else
    let nth_elm = List.nth original_list idx in 
    let nth_modified_elm = match nth_elm with
    | Evaluterutil.Variable(x) -> x
    | Evaluterutil.Lists (list) -> 
      let string_list = proc_Lists [] list 0 in
      String.concat "," string_list
    in
    variable_list_to_list (reuslt_list@[nth_modified_elm]) original_list (idx+1)  
;;

let evalute original_document hashtbl = 
  let literals = search_literals original_document in
  let rec expand_docuemnt  modified_docuemtns idx start= 
    if idx = List.length literals then
      let modified = modified_docuemtns ^ (String.sub original_document start (String.length original_document - start)) in
      modified
    else
      let nth_literal = List.nth literals idx in 
      let (_, results) = Evaluterutil.parse_and_evalute nth_literal._content "" [] [] 0 false in
      let modified_to_string_list = variable_list_to_list [] results 0 in
      (*until not supported if and for, so only use 0 idx elm *)
      let evaluted = List.nth modified_to_string_list 0 in

      let modified = modified_docuemtns ^ String.sub original_document start (nth_literal._start - start) ^ evaluted in
      expand_docuemnt modified (idx+1) (nth_literal._end+2)
  in
  expand_docuemnt "" 0 0
;;
end