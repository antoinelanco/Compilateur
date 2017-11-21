open Format

let usage = "usage: compilo [options] file.a6m"

(* Par défaut : génération de code MIPS sans optimisation. *)
let interpret      = ref false
let reg_allocation = ref false
let dead_code_elim = ref false
let prebuilt_frontend = ref false
let input = ref 0

let spec =
  [ "-r", Arg.Set reg_allocation, "  with register allocation";
    "-dce", Arg.Set dead_code_elim, "  with dead code elimination";
    "-O", Arg.Tuple [Arg.Set reg_allocation; Arg.Set dead_code_elim],
    "  full optimisation";
    "-i", Arg.Tuple [Arg.Set_int input; Arg.Set interpret],
    "  interpreter only";
    "-frontend", Arg.Set prebuilt_frontend, "  use prebuilt frontend"
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".a6m") then
      raise (Arg.Bad "no .a6m extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  Printf.printf "-------- Debut --------\n";
  Printf.printf "-------- SourceParser --------\n";
  let p  =  SourceParser.prog SourceLexer.token lb
  (* if !prebuilt_frontend
     then PrebuiltParser.main PrebuiltLexer.token lb
     else SourceParser.main SourceLexer.token lb *)
  in
  close_in c;
  Printf.printf "-------- SourceTypeChecker --------\n";
  SourceTypeChecker.typecheck_prog p;
  if !interpret
  then let _ = SourceInterpreter.eval_main p !input in ()
  else begin
    Printf.printf "-------- SourcetoUntyped --------\n";
    let p = SourcetoUntyped.erase_prog p in
    Printf.printf "-------- UntypedtoGoto --------\n";
    let p = UntypedtoGoto.destructure_prog p in
    Printf.printf "-------- GototoIr --------\n";
    let p = GototoIr.flatten_prog p in
    (* Code à réintégrer à la séance 3 *)
    let p =
      if   !dead_code_elim
      then (Printf.printf "-------- IrDeadCodeElim --------\n";IrDeadCodeElim.dce p)
      else p
    in
    Printf.printf "-------- IrtoAllocated --------\n";
    let p = IrtoAllocated.allocate_main !reg_allocation p in
    Printf.printf "-------- AllocatedtoMips --------\n";
    let asm = AllocatedtoMips.generate_prog p in
    Printf.printf "-------- Fin --------\n";
    let output_file = (Filename.chop_suffix file ".a6m") ^ ".asm" in
    let out = open_out output_file in
    let outf = formatter_of_out_channel out in
    Mips.print_program outf asm;
    pp_print_flush outf ();
    close_out out;
  end;
  exit 0
