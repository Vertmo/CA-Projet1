open Bytecode

type mlvalue = Int of int
             | Unit
             | Closure of int * mlvalue list
             | Pc of int
             | Label of string
             | ExtraArgs of int
             | Env of mlvalue list
             | Block of mlvalue array
             | StackPointer of mlvalue list option

let rec string_of_mlvalue = function
  | Int i -> string_of_int i
  | Unit -> "()"
  | Closure (pc, _) -> Printf.sprintf "closure %d <env>" pc
  | Pc i -> Printf.sprintf "pc %d " i
  | Label l -> Printf.sprintf "label %s" l
  | ExtraArgs n -> Printf.sprintf "extra_args %d" n
  | Env _ -> "env"
  | Block a -> Printf.sprintf "(%s)" (String.concat ","
                                        (Array.to_list (Array.map string_of_mlvalue a)))
  | StackPointer _ -> "sp"

type vm_state = {
  mutable prog: ins list;
  mutable stack: mlvalue list;
  mutable env: mlvalue list;
  mutable pc: int;
  mutable accu: mlvalue;
  mutable extra_args: int;
  mutable trap_sp: mlvalue list option;
}

let state: vm_state = {
  prog = [];
  stack = [];
  env = [];
  pc = 0;
  accu = Unit;
  extra_args = 0;
  trap_sp = None;
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

let eval_opcode o = try
  match o with
  | CONST n -> state.accu <- Int n
  | PUSH -> state.stack <- state.accu::state.stack
  | POP -> state.stack <- List.tl state.stack
  | PRIM p -> state.accu <- apply_prim p
  | BRANCH l -> state.pc <- (find_pc_of_label l 0 state.prog)
  | BRANCHIFNOT l -> (match state.accu with
      | Int 0 -> state.pc <- (find_pc_of_label l 0 state.prog)
      | Int _ | Block _ -> ()
      | _ -> failwith "Should not happen (BRANCHIFNOT)")
  | ACC i -> state.accu <- (List.nth state.stack i)
  | ENVACC i -> state.accu <- (List.nth state.env i)
  | CLOSURE (l, n) -> (if n > 0 then state.stack <- state.accu::state.stack;
                       let (env, newStack) = pop_n n state.stack in
                       state.stack <- newStack;
                       state.accu <- Closure ((find_pc_of_label l 0 state.prog), env))
  | APPLY n -> let (args, newStack) = pop_n n state.stack in
    state.stack <- args@[Pc (state.pc+1)]@[Env state.env]@[ExtraArgs state.extra_args]@newStack;
    state.extra_args <- n-1;
    (match state.accu with
     | Closure (pc, env) -> (state.pc <- pc; state.env <- env)
     | _ -> failwith "Should not happen (APPLY)")
  | RETURN n -> let (_, newStack) = pop_n n state.stack in
    state.stack <- newStack;
    if(state.extra_args = 0) then (
      let pc = (List.hd state.stack) and newStack = (List.tl state.stack) in
      let env = (List.hd newStack) and newStack = (List.tl newStack) in
      let extra_args = (List.hd newStack) and newStack = (List.tl newStack) in
      state.stack <- newStack;
      (match (pc, env, extra_args) with
       | (Pc pc), (Env e), (ExtraArgs ext) -> (state.pc <- pc; state.env <- e; state.extra_args <- ext)
       | _ -> failwith "Should not happen (RETURN)"))
    else (
      state.extra_args <- state.extra_args-1;
      match state.accu with
      | Closure (pc, env) -> state.pc <- pc; state.env <- env
      | _ -> failwith "Should not happen (RETURN)"
    )
  | STOP -> (print_endline "----------Fin du programme. Valeur de retour :-----------";
             print_endline (string_of_mlvalue state.accu); exit 0)
  | CLOSUREREC (l, n) -> (if n > 0 then state.stack <- state.accu::state.stack;
                          let (env, newStack) = pop_n n state.stack in
                          state.stack <- newStack;
                          let pc = find_pc_of_label l 0 state.prog in
                          let c = Closure (pc, ((Pc pc)::env)) in
                          state.accu <- c;
                          state.stack <- c::state.stack)
  | OFFSETCLOSURE -> (match (List.hd (state.env)) with
      | Pc pc -> state.accu <- Closure (pc, state.env)
      | _ -> failwith "Should not happen (OFFSETCLOSURE)")
  | GRAB n -> if state.extra_args >= n then state.extra_args <- state.extra_args - n
    else (
      let (env, newStack) = pop_n (state.extra_args+1) state.stack in state.stack <- newStack;
      state.accu <- Closure (state.pc-1, (Env state.env)::env);
      let pc = (List.hd state.stack) and newStack = (List.tl state.stack) in
      let env = (List.hd newStack) and newStack = (List.tl newStack) in
      let extra_args = (List.hd newStack) and newStack = (List.tl newStack) in
      state.stack <- newStack;
      match (pc, env, extra_args) with
      | (Pc pc), (Env env), (ExtraArgs ext) -> (
          state.pc <- pc;  state.env <- env; state.extra_args <- ext;
        )
      | _ -> failwith "Should not happen (GRAB)"
    )
  | RESTART -> let n = List.length state.env in
    let (env, _) = pop_n n state.env in
    state.stack <- (List.tl env)@state.stack;
    (match (List.hd env) with
     | Env e -> state.env <- e
     | _ -> failwith "Should not happen (RESTART)");
    state.extra_args <- state.extra_args + (n-1)

  (* Appterm *)
  | APPTERM (n, m) ->
    let (args, newStack) = pop_n n state.stack in
    let (_, newStack) = pop_n (m-n) newStack in
    state.stack <- args@newStack;
    (match state.accu with
     | Closure (pc, env) -> (state.pc <- pc; state.env <- env)
     | _ -> failwith "Should not happen (APPTERM)");
    state.extra_args <- state.extra_args + (n-1)

  (* Blocs *)
  | MAKEBLOCK n -> let b = Array.make n Unit in
    if (n > 0) then (
      b.(0) <- state.accu;
      let (vals, newStack) = pop_n (n-1) state.stack in state.stack <- newStack;
      List.iteri (fun i v -> b.(i+1) <- v) vals;
    ); state.accu <- Block b
  | GETFIELD n -> (match state.accu with
      | Block b -> state.accu <- b.(n)
      | _ -> failwith "Should not happen (GETFIELD)")
  | VECTLENGTH -> (match state.accu with
      | Block b -> state.accu <- Int (Array.length b)
      | _ -> failwith "Should not happen (VECTLENGTH)")
  | GETVECTITEM ->
    let n = (List.hd state.stack) and newStack = (List.tl state.stack) in
    state.stack <- newStack;
    (match state.accu, n with
     | Block b, Int n -> state.accu <- b.(n)
     | _ -> failwith "Should not happen (GETVECTITEM)")
  | SETFIELD n ->
    let v = (List.hd state.stack) and newStack = (List.tl state.stack) in
    state.stack <- newStack;
    (match state.accu with
     | Block b -> b.(n)<-v
     | _ -> failwith "Should not happen (SETFIELD)")
  | SETVECTITEM ->
    let n = (List.hd state.stack) and newStack = (List.tl state.stack) in
    let v = (List.hd newStack) and newStack = (List.tl newStack) in
    state.stack <- newStack;
    (match state.accu, n with
    | Block b, Int n -> b.(n)<-v
    | _ -> failwith "Should not happen (SETVECTITEM)"
    );
    state.accu <- Unit;
  | ASSIGN n ->
    let (temp, newStack) = pop_n n state.stack in
    state.stack <- temp@[state.accu]@(List.tl newStack)

  (* Exceptions *)
  | PUSHTRAP l ->
    state.stack <- [Pc (find_pc_of_label l 0 state.prog);
                    StackPointer state.trap_sp;
                    Env state.env;
                    ExtraArgs state.extra_args]@state.stack;
    state.trap_sp <- Some state.stack
  | POPTRAP -> let newStack = (List.tl state.stack) in
    let trap_sp = (List.hd newStack) and newStack = (List.tl newStack) in
    state.stack <- (List.tl (List.tl newStack));
    (match trap_sp with
     | StackPointer sp -> state.trap_sp <- sp
     | _ -> failwith "Should not happen (PUSHTRAP)")

  | RAISE -> (match state.accu with
      | Int i -> (match state.trap_sp with
          | None -> print_endline (Printf.sprintf "Exception: %d" i); exit 1
          | Some sp -> state.stack <- sp;
            let pc = (List.hd state.stack) and newStack  = (List.tl state.stack) in
            let trap_sp = (List.hd newStack) and newStack = (List.tl newStack) in
            let env = (List.hd newStack) and newStack = (List.tl newStack) in
            let extra_args = (List.hd newStack) and newStack = (List.tl newStack) in
            state.stack <- newStack;
            (match pc, trap_sp, env, extra_args with
             | (Pc pc), (StackPointer sp), (Env env), (ExtraArgs extra_args) ->
               state.pc <- pc; state.trap_sp <- sp; state.env <- env; state.extra_args <- extra_args
             | _ -> failwith "Should not happen (RAISE)"))
     | _ -> failwith "Should not happen (RAISE)")
  with Failure e -> failwith (Printf.sprintf "Exception evaluating '%s': %s" (string_of_opcode o) e)

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
