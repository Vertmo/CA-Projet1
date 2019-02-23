type prim = Add | Sub | Mult | Div
          | Or | And | Not
          | Eq | Diff | Lt | Lte | Gt | Gte
          | Print

let string_of_prim = function
  | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
  | Or -> "or" | And -> "and" | Not -> "not"
  | Eq -> "="  | Diff -> "<>"
  | Lt -> "<" | Lte -> "<=" | Gt -> ">" | Gte -> ">="
  | Print -> "print"

let arity_of_prim = function
  | Add | Sub | Mult | Div | Or | And | Eq | Diff
  | Lt | Lte | Gt | Gte -> 2
  | Not | Print -> 1

type opcode = CONST of int
            | PRIM of prim
            | BRANCH of string
            | BRANCHIFNOT of string
            | PUSH
            | POP
            | ACC of int
            | ENVACC of int
            | CLOSURE of string * int
            | APPLY of int
            | RETURN of int
            | STOP
            | CLOSUREREC of string * int
            | OFFSETCLOSURE

let string_of_opcode = function
  | CONST n -> Printf.sprintf "CONST %d" n
  | PRIM p -> Printf.sprintf "PRIM %s" (string_of_prim p)
  | BRANCH l -> Printf.sprintf "BRANCH %s" l
  | BRANCHIFNOT l -> Printf.sprintf "BRANCHIFNOT %s" l
  | PUSH -> "PUSH"
  | POP -> "POP"
  | ACC i -> Printf.sprintf "ACC %d" i
  | ENVACC i -> Printf.sprintf "ENVACC %d" i
  | CLOSURE (l, n) -> Printf.sprintf "CLOSURE %s,%d" l n
  | APPLY n -> Printf.sprintf "APPLY %d" n
  | RETURN n -> Printf.sprintf "RETURN %d" n
  | STOP -> "STOP"
  | CLOSUREREC (l, n) -> Printf.sprintf "CLOSUREREC %s,%d" l n
  | OFFSETCLOSURE -> "OFFSETCLOSURE"

type ins =
  | Anon of opcode
  | Labeled of string * opcode

let string_of_ins = function
  | Anon o -> Printf.sprintf "\t%s" (string_of_opcode o)
  | Labeled (l, o) -> Printf.sprintf "%s:\t%s" l (string_of_opcode o)

let string_of_bc bc =
  String.concat "\n" (List.map string_of_ins bc)
