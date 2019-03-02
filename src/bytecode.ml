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
            | GRAB of int
            | RESTART
            (* Appterm *)
            | APPTERM of int * int
            (* Blocs de valeurs *)
            | MAKEBLOCK of int
            | GETFIELD of int
            | VECTLENGTH
            | GETVECTITEM
            | SETFIELD of int
            | SETVECTITEM
            | ASSIGN of int
            (* Exceptions *)
            | PUSHTRAP of string
            | POPTRAP
            | RAISE

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
  | GRAB n -> Printf.sprintf "GRAB %d" n
  | RESTART -> "RESTART"
  (* Appterm *)
  | APPTERM (n, m) -> Printf.sprintf "APPTERM %d,%d" n m
  (* Blocs *)
  | MAKEBLOCK n -> Printf.sprintf "MAKEBLOCK %d" n
  | GETFIELD n -> Printf.sprintf "GETFIELD %d" n
  | VECTLENGTH -> "VECTLENGTH"
  | GETVECTITEM -> "GETVECTITEM"
  | SETFIELD n -> Printf.sprintf "SETFIELD %d" n
  | SETVECTITEM -> Printf.sprintf "SETVECTITEM"
  | ASSIGN n -> Printf.sprintf "ASSIGN %d" n
  (* Exceptions *)
  | PUSHTRAP l -> Printf.sprintf "PUSHTRAP %s" l
  | POPTRAP -> "POPTRAP"
  | RAISE -> "RAISE"

type ins =
  | Anon of opcode
  | Labeled of string * opcode

let string_of_ins = function
  | Anon o -> Printf.sprintf "\t%s" (string_of_opcode o)
  | Labeled (l, o) -> Printf.sprintf "%s:\t%s" l (string_of_opcode o)

let string_of_bc bc =
  String.concat "\n" (List.map string_of_ins bc)

(** Transform every instance of APPLY n; RETURN m-n into APPTERM n,m *)
let transformAppterm bc =
  List.rev (fst (List.fold_left (fun (nbc, lastApply) ins -> match (lastApply, ins) with
      (* Capturing the previous APPLY *)
      | _, Anon (APPLY i) -> nbc, Some (Anon (APPLY i))
      | _, Labeled (l, (APPLY i)) -> nbc, Some (Labeled (l, (APPLY i)))

      (* Actual transformation *)
      | Some (Anon (APPLY n)), Anon (RETURN m) -> (Anon (APPTERM (n,n+m))::nbc, None)
      | Some (Anon (APPLY n)), Labeled (_, (RETURN m)) -> (Anon (APPTERM (n,n+m))::nbc, None)
      | Some (Labeled (l, (APPLY n))), Anon (RETURN m) -> (Labeled (l, (APPTERM (n,n+m)))::nbc, None)
      | Some (Labeled (l, (APPLY n))), Labeled (_, (RETURN m)) -> (Labeled (l, (APPTERM (n,n+m)))::nbc, None)

      (* Default cases *)
      | Some (Anon (APPLY i)), ins -> (ins::(Anon (APPLY i))::nbc, None)
      | Some (Labeled (l, (APPLY i))), ins -> (ins::(Labeled (l, (APPLY i)))::nbc, None)
      | Some pi, ins -> (ins::pi::nbc, None)
      | None, ins -> (ins::nbc, None)
    ) ([], None) bc))
