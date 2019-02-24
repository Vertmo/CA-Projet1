open Bytecode

let read_lines ic =
  let lines = ref [] in
  try
    while true; do
      lines := input_line ic :: !lines
    done; !lines
  with End_of_file ->
    List.rev !lines ;;

let prim_of_string = function
  | "+" -> Add | "-" -> Sub | "*" -> Mult | "/" -> Div
  | "or" -> Or | "and" -> And | "not" -> Not
  | "=" -> Eq | "<>" -> Diff
  | "<" -> Lt | "<=" -> Lte | ">" -> Gt | ">=" -> Gte
  | "print" -> Print
  | s -> failwith (Printf.sprintf "Unrecognized primitive %s" s)

let opcode_of_string_and_args s args = match s with
  | "CONST" -> CONST (int_of_string (List.nth args 0))
  | "PRIM" -> PRIM (prim_of_string (List.nth args 0))
  | "BRANCH" -> BRANCH (List.nth args 0)
  | "BRANCHIFNOT" -> BRANCHIFNOT (List.nth args 0)
  | "PUSH" -> PUSH
  | "POP" -> POP
  | "ACC" -> ACC (int_of_string (List.nth args 0))
  | "ENVACC" -> ENVACC (int_of_string (List.nth args 0))
  | "CLOSURE" -> CLOSURE ((List.nth args 0), (int_of_string (List.nth args 1)))
  | "APPLY" -> APPLY (int_of_string (List.nth args 0))
  | "RETURN" -> RETURN (int_of_string (List.nth args 0))
  | "STOP" -> STOP
  | "CLOSUREREC" -> CLOSUREREC ((List.nth args 0), (int_of_string (List.nth args 1)))
  | "OFFSETCLOSURE" -> OFFSETCLOSURE
  | "GRAB" -> GRAB (int_of_string (List.nth args 0))
  | "RESTART" -> RESTART
  | s -> failwith (Printf.sprintf "Unrecognized opcode %s" s)

let scan l =
  let sl = String.split_on_char '\t' l in
  let opcodeAndArgs = (String.split_on_char ' ' (List.nth sl 1)) in
  let args = if (List.length opcodeAndArgs > 1)
    then (String.split_on_char ',' (List.nth opcodeAndArgs 1))
    else [] in
  let opcode = opcode_of_string_and_args (List.nth opcodeAndArgs 0) args in
  if(List.nth sl 0 = "") then Anon opcode
  else (
    let numS = String.sub (List.nth sl 0) 0 ((String.length (List.nth sl 0))-1) in
    Labeled (numS, opcode)
  )

let parse f =
  let lines = read_lines f in
  List.map scan lines
