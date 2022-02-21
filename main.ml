let ifile = ref ""
let ofile = ref ""
let show_tokens = ref false
let show_ast = ref false
let show_type = ref false
let show_ir = ref false
let show_bytecode = ref false
let set_file f s = f := s

let usage = "usage: ml2cpp file.ml"
let options =
  ["--show-tokens", Arg.Set show_tokens,
     " print all tokens and stop";
   "--show-ast", Arg.Set show_ast,
     " show abstract syntax tree";
   "--show-type", Arg.Set show_type,
     " show abstract syntax tree with types";
   "--show-ir", Arg.Set show_ir,
     " show intermediate representation";
   "--show-bc", Arg.Set show_bytecode,
     " show bytecode";
  ]

let () =
  Arg.parse options (set_file ifile) usage;
  
  if !ifile = "" then begin
    Format.fprintf Format.err_formatter "no input file.@.";
    Arg.usage options usage;
    exit 1
  end;

  if not (Filename.check_suffix !ifile ".ml") then begin
    Format.fprintf Format.err_formatter "filename must have .ml suffix.@.";
    Arg.usage options usage;
    exit 1
  end;

  ofile := (Filename.chop_extension !ifile) ^ ".bc";

  let input = open_in !ifile in

  let lexbuf = Lexing.from_channel input in

  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname = !ifile;
    Lexing.pos_lnum  = 1;
    Lexing.pos_bol   = 0;
    Lexing.pos_cnum  = 0
  };
  try
    let prog = Parser.program Lexer.token lexbuf in
    close_in input;
    (*Format.pp_set_margin Format.std_formatter 15;*)
    let open Format in 
    if !show_ast then fprintf std_formatter "@[<hov 2>/*@ %a@ */@]@." Pp.pp_program prog;        
    let tprog = Typechecking.typeof prog in
    if !show_type then begin       
      fprintf std_formatter "@[<hov 2>/*@ %a :@ %a@ */@]@." 
        Pp.pp_program prog 
        Pp.pp_typ tprog.typ;
      fprintf std_formatter "@[<hov 2>/*@ %a@ */@]@." Pp.pp_tprogram tprog     
    end;
    let ir = To_ir.tprogram_to_ir tprog in
    if !show_ir then begin 
      fprintf std_formatter "@[<v>%a@]@." Pp.pp_ir ir        
    end; 
    let res = Ir_simulator.exec ir in
    fprintf std_formatter "@[<hov 2>result:@ %a@]@."
        Pp.pp_ir_value res;
    let bytecode = To_bytecode.ir_to_bytecode ir in    
    if !show_bytecode then begin 
      fprintf std_formatter "@[<v>%a@]@."
        (pp_print_list ~pp_sep:pp_print_cut Pp.pp_bytecode) bytecode 
    end;
    let output = open_out_bin !ofile in
    set_formatter_out_channel output;
    List.to_seq bytecode 
    |> Bytes.of_seq 
    |> fprintf std_formatter "%a@?" pp_print_bytes;
    close_out output 
  with
    | Lexer.Error msg ->
        Format.fprintf Format.err_formatter "Lexical error: %s@." msg;
        exit 1
    | Parser.Error ->
        Format.fprintf Format.err_formatter "Syntax error at line %d@." 
          (Lexing.lexeme_start_p lexbuf).Lexing.pos_lnum;
        exit 1 
    | Typechecking.Error msg ->
        Format.fprintf Format.err_formatter "Type checking error: %s@." msg;
        exit 1
    | To_ir.Error msg ->
      Format.fprintf Format.err_formatter "Intermediate representation error: %s@." msg;
      exit 1
