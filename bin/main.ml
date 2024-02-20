let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let rec lex_files token_lists files =
  match files with
  | [] -> token_lists
  | file :: rest ->
      lex_files (token_lists @ [ Lexer.lex (read_whole_file file) file ]) rest

let usage_msg = "mayo -o <output> <sources>... "
let input_files = ref []
let output_file = ref ""
let anon_fun filename = input_files := filename :: !input_files
let speclist = [ ("-o", Arg.Set_string output_file, "Set output file name") ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  let token_lists = lex_files [] !input_files in

  if List.length token_lists == 0 then Log.print_fatal "no input files";
  for i = 0 to List.length token_lists - 1 do
    let tokens = Util.unwrap (List.nth_opt token_lists i) in
    let ast = Parser.parse_expr (ref tokens) in
    print_endline (Lexer.tokens_to_string_dbg tokens);
    print_endline (Parser.ast_to_string ast);
    Parser.print_ast_span ast.span (read_whole_file ast.span.file)
    (* match List.nth_opt tokens 4 with *)
    (* | Some t -> Lexer.print_token_span (t, file_content) *)
    (* | _ -> () *)
  done
