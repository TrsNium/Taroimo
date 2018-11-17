
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
let documents =  read_whole_channel chan;;

(*Printf.printf "%s"  documents;;*)

(* TODO: surround with module*)
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

search_literals documents;;

type variable_type = 
  | Nothing
  | Variable of string
  | String_variable of string
  | Int_variable of int
  | List_variable of 'variable_type list

type method_type = 
  | Nothing
  | Split of func
  |

type content_type = 
  | Varibale of variable_type
  | Method of string 
  | String_type of 
  | 


type hoge = 
  | Contents of string 
  | Child of hoge ;;

type structs = {_content_type: content_type, }


(* TODO: add evaluter module struct
  And this module include above module as child module ? *)
module Evaluter = struct
  let parse_block searched_literals = 
    Printf.printf "%s" "hoge";
  
end 
