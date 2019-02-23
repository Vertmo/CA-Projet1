open Bytecode

type mlvalue = Int of int
             | Unit
             | Closure of string * mlvalue list
             | Pc of int | Label of string
             | Env of mlvalue list

let string_of_mlvalue = function
  | Int i -> string_of_int i
  | Unit -> "()"
  | Closure (s, _) -> Printf.sprintf "closure %s <env>" s
  | Pc i -> Printf.sprintf "pc %d " i
  | Label l -> Printf.sprintf "label %s" l
  | Env _ -> "env"

type vm_state = {
  mutable prog: ins list;
  mutable stack: mlvalue list;
  mutable env: mlvalue list;
  mutable pc: int;
  mutable accu: mlvalue;
}

let state: vm_state = {
  prog = [];
  stack = [];
  env = [];
  pc = 0;
  accu = Unit;
}

let rec pop_n n = function
  | [] -> if n = 0 then ([], []) else failwith "Should not happen"
  | s when n = 0 -> ([], s)
  | t::q -> let (e, s) = pop_n (n-1) q in (t::e, s)

let apply_bin_prim p arg1 arg2 =
  let (i1, i2) = match (arg1, arg2) with (Int i1, Int i2) -> (i1, i2) | _ -> failwith "Should not happen" in
  match p with
  | Add -> Int (i1 + i2) | Sub -> Int (i1 - i2) | Mult -> Int (i1 * i2) | Div -> Int (i1 / i2)
  | Eq -> Int (if i1 = i2 then 1 else 0) | Diff -> Int (if i1 <> i2 then 1 else 0)
  | Lt -> Int (if i1 < i2 then 1 else 0) | Lte -> Int (if i1 <= i2 then 1 else 0)
  | Gt -> Int (if i1 > i2 then 1 else 0) | Gte -> Int (if i1 >= i2 then 1 else 0)
  | Or -> Int (if i1 > 0 || i2 > 0 then 1 else 0) | And -> Int (if i1 > 0 && i2 > 0 then 1 else 0)
  | _ -> failwith "Should not happen"

let apply_prim p =
  if (arity_of_prim p) = 2 then (
    let arg1 = state.accu and arg2 = (List.hd state.stack) in
    state.stack <- List.tl state.stack;
    apply_bin_prim p arg1 arg2
  ) else (
    let arg = state.accu in match p with
    | Not -> (match arg with Int i -> Int (if i > 0 then 0 else 1) | _ -> failwith "Should not happen")
    | Print -> (match arg with Int i -> print_char (char_of_int i); print_newline (); Unit
                             | _ -> failwith "Should not happen")
    | _ -> failwith "Should not happen"
  )

let rec find_pc_of_label l c = function
  | [] -> failwith "Not found"
  | (Labeled (l', _))::_ when l' = l -> c
  | _::q -> find_pc_of_label l (c+1) q

let eval_opcode = function
  | CONST n -> state.accu <- Int n
  | PUSH -> state.stack <- state.accu::state.stack
  | POP -> state.stack <- List.tl state.stack
  | PRIM p -> state.accu <- apply_prim p
  | BRANCH l -> state.pc <- (find_pc_of_label l 0 state.prog)
  | BRANCHIFNOT l -> (match state.accu with
      | Int 0 -> state.pc <- (find_pc_of_label l 0 state.prog)
      | Int _ -> ()
      | _ -> failwith "Should not happen")
  | ACC i -> state.accu <- (List.nth state.stack i)
  | ENVACC i -> state.accu <- (List.nth state.env i)
  | CLOSURE (l, n) -> (if n > 0 then state.stack <- state.accu::state.stack;
                       let (env, newStack) = pop_n n state.stack in
                       state.stack <- newStack;
                       state.accu <- Closure (l, env))
  | APPLY n -> let (args, newStack) = pop_n n state.stack in
    state.stack <- args@[Pc (state.pc+1)]@[Env state.env]@newStack;
    (match state.accu with
     | Closure (l, env) -> (state.pc <- (find_pc_of_label l 0 state.prog); state.env <- env)
     | _ -> failwith "Should not happen")
  | RETURN n -> let (_, newStack) = pop_n n state.stack in
    let pc = (List.hd newStack) and newStack = (List.tl newStack) in
    let env = (List.hd newStack) and newStack = (List.tl newStack) in
    state.stack <- newStack;
    (match (pc, env) with
     | (Pc pc), (Env e) -> (state.pc <- pc; state.env <- e)
     | _ -> failwith "Should not happen")
  | STOP -> (print_endline "----------Fin du programme. Valeur de retour :-----------";
             print_endline (string_of_mlvalue state.accu); exit 0)
  | CLOSUREREC (l, n) -> (if n > 0 then state.stack <- state.accu::state.stack;
                          let (env, newStack) = pop_n n state.stack in
                          state.stack <- newStack;
                          state.accu <- Closure (l, ((Label l)::env)))
  | OFFSETCLOSURE -> (match (List.hd (state.env)) with
      | Label l -> state.accu <- Closure (l, state.env)
      | _ -> failwith "Should not happen")
  (* | o -> failwith (Printf.sprintf "Opcode %s not yet implemented" (string_of_opcode o)) *)

let eval_ins = function
  | Anon o -> eval_opcode o
  | Labeled (_, o) -> eval_opcode o

let eval_bc bc =
  state.prog <- bc;
  let n = List.length bc in
  while (state.pc < n) do
    let oldPc = state.pc in
    eval_ins (List.nth bc state.pc);
    if(oldPc = state.pc) then state.pc <- state.pc + 1
  done
