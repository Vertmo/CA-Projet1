let usage = "usage: " ^ Sys.argv.(0) ^ " [-parse] [-eval] <input_file>"

type step = Parse | Eval

let step = ref Eval

let speclist = [
  ("-parse", Arg.Unit (fun () -> step := Parse), ": only parse the program");
  ("-eval", Arg.Unit (fun () -> step := Eval), ": evaluate the program (default)")
]

let main filename step =
  let ic = open_in filename in
  let bc = Parser.parse ic in
  close_in ic;
  if(step = Parse) then print_endline (Bytecode.string_of_bc bc)
  else Eval.eval_bc bc

let _ = Arg.parse speclist (fun x -> main x !step) usage
