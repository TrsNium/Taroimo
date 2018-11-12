
open Str;;
open Printf;;

let file = "hoge.txt";;

let read_whole_channel chan =
  let buf = Buffer.create 4096 in
  let rec loop () =
    let newline = input_line chan in
    Buffer.add_string buf newline;
    Buffer.add_char buf '\n';
    loop ()
  in
  try
    loop ()
  with
    End_of_file -> Buffer.contents buf;;

(* load_file file; *)
let chan = open_in file;;
let buf =  read_whole_channel chan;;

Printf.printf "%s"  buf;;

let regex_patterns = [|"{{\s*[a-zA-Z0-9]+\s*}}"; ""|];;
let compiled_regex_patterns  patterns = 
  let pattern_num = Array.length patterns in   
  let compiled_lists = [||] in
  let rec compile_pattern idx =  
    if idx > 0 then
      let new_idx = idx - 1 in
      let content = Array.get patterns new_idx in 
      let compiled = [|Str.regexp content|] in
      compiled_lists = Array.append compiled_lists compiled;
      
      compile_pattern new_idx;
  in
  compile_pattern pattern_num;;

let search_block document = 
  let document_length = String.length document in 
  let rec find_out_duplicate_char char start flag= 
    let nth_char = String.get document start in
    if start == document_length then raise "Not Found!"
    else if flag == True && nth_char == char then start - 1
    else if flag == True && nth_char != char then find_out_duplicate_char char start+1 False
    else find_out_duplicate_char char start+1 True
  in

  let rec find_out_start_phrase document start_phrase_list = 
    
  in