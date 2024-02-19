let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let () =
  let file_content = read_whole_file "./test.txt" in
  let tokens = Lexer.lex file_content in
  print_endline (Lexer.tokens_to_string_dbg tokens);
  match List.nth_opt tokens 4 with
  | Some t -> Lexer.print_token_span (t, file_content)
  | _ -> print_endline "ERROR: could not get token"
