
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

type 'a variable_type = 
  | Variable of 'a 
  | List_variable of 'a variable_type list;;

type method_type = 
  | Nothing of variable_type 
  | Split of Str.regex * (string variable_type)
  | Join of  string * (string variable_type);;(* String.concat "," ['test1, test2'] *)

(* ==, != , >, <, >=, <= *)
type comparative_operator =  Eq | NEq | Greater | Less | Greater_or_Eq | Less_or_Eq ;;
type if_of_struct = {left_node: method_type ; conmpareter: comparative_operator; right_node:method_type};;
type for_of_struct = {outputs_node: variable_type; inputs_node: method_type};;

type literal_type = 
  | Nothing of method_type
  | If (*of if_of_struct*)
  | For (*of for_of_struct*)
  | Endif
  | Endfor
;;

type block_of_struct = 
  | InBlock_Literals of literal_types list 
  | Child_Block of block_of_struct list 
  | InBlock_Variable_Names of (string, '_b) Hashtbl.t
;;

(* TODO: add evaluter module struct
  And this module include above module as child module ? *)
module Evaluter = struct
  
  let set_variable variable = 
    print_endline variable

  let parse_element element = 
    parse_chars 0 "feaofjewao"

  (*
  let parse_if_element elments =
    printline elements;

  let parse_for_elment elements = 
    ();
  *)

  let parser_nothing elements = 
    let element = String.concat " " elements in
    ()
  
  let parse_block searched_literals = 
    let detect_literal_type content = 
      let words =  String.split_on_char ' ' content in
      let elements = List.filter (fun x-> x <> "") words in 
      let first_literal = elements.get 0 in 
      let literal_type () = if first_literal = "if" then For
                         else if first_literal = "for" then If
                         else if first_literal = "endif" then Endif
                         else if first_literal = "endfor" then Endfor
                         else parse_nothing elements
      in
      literal_type ();
    in
    
    
    let rec add_block literal flag idx =
      let content = searched_literals.get(idx).content in
      let literal_type = detect_literal_type content in
    in
end