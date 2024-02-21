type ast_node_kind =
  | Program of ast_node list
  | LetStatment of string * ast_node
  | BinOp of ast_node * Lexer.operator * ast_node
  | Ident of string
  | StrLit of string
  | Number of Lexer.number
  | Funcall of ast_node * ast_node list

and ast_node = { kind : ast_node_kind; span : span }
and span = { f : Lexer.position; t : Lexer.position; file : string }

let span_from_to_token (from_t : Lexer.token) (to_t : Lexer.token) =
  {
    f = { from_t.pos with column = from_t.pos.column - from_t.len };
    t = to_t.pos;
    file = from_t.pos.file;
  }

let span_from_pos_and_len (pos : Lexer.position) len =
  { f = { pos with column = pos.column - len }; t = pos; file = pos.file }

let span_from_to_node (from_n : ast_node) (to_n : ast_node) =
  { f = from_n.span.f; t = to_n.span.t; file = from_n.span.file }

let span_single (t : Lexer.token) =
  {
    f = { t.pos with column = t.pos.column - t.len };
    t = t.pos;
    file = t.pos.file;
  }

let rec ast_to_string ast =
  match ast.kind with
  | Program items -> List.map ast_to_string items |> String.concat ", "
  | LetStatment (name, value) ->
      Printf.sprintf "let %s = %s" name (ast_to_string value)
  | BinOp (lhs, op, rhs) ->
      Printf.sprintf "BinOp(%s, %s, %s)" (ast_to_string lhs)
        (Lexer.operator_to_string op)
        (ast_to_string rhs)
  | Ident s -> s
  | StrLit string -> Printf.sprintf "\"%s\"" string
  | Number num -> Lexer.num_to_string num
  | Funcall (id, args) ->
      let args_str = List.map ast_to_string args in
      Printf.sprintf "%s(%s)" (ast_to_string id) (String.concat ", " args_str)

let should_keep_going (tokens : Lexer.token list) min_prec =
  match tokens with
  | token :: _ -> (
      match token.kind with
      | Lexer.Operator op -> Lexer.op_prec op >= min_prec
      | _ -> false)
  | _ -> false

let span_len s = s.t.column - s.f.column

let print_ast_span s file =
  print_endline
    (Log.bold_text
       (Printf.sprintf "File: %s, line %i-%i, column %i-%i" s.file s.f.line
          s.t.line s.f.column s.t.column));
  Printf.printf "%i | %s\n" s.f.line (Util.get_line_from_string s.f.line file);
  let len = String.length (Printf.sprintf "%i | " s.f.line) in
  let spaces = String.make (len + s.f.column) ' ' in
  let carrots = Log.red_text (String.make (span_len s) '^') in
  Printf.printf "%s%s\n" spaces carrots;
  ()

let print_ast_span_no_file span =
  print_ast_span span (Util.read_whole_file span.file)

let print_ast_node_span n =
  print_ast_span n.span (Util.read_whole_file n.span.file)

let rec parse_args (tokens : Lexer.token list ref) args =
  match !tokens with
  | { kind = Lexer.Punct Lexer.LParen; _ } :: rest ->
      tokens := rest;
      parse_args tokens (args @ [ parse_expr tokens ])
  | { kind = Lexer.Punct Lexer.Comma; _ } :: rest ->
      tokens := rest;
      parse_args tokens (args @ [ parse_expr tokens ])
  | { kind = Lexer.Punct Lexer.RParen; _ } :: rest ->
      tokens := rest;
      args
  | token :: _ ->
      Lexer.print_token_span_no_file token;
      failwith "Parsing error"
  | _ ->
      Log.print_fatal "expected function call but got EOF";
      failwith "Parsing error"

and parse_primary (tokens : Lexer.token list ref) =
  match !tokens with
  | token :: rest -> (
      match token.kind with
      | Lexer.Ident id -> (
          tokens := rest;
          let id_node = { kind = Ident id; span = span_single token } in
          match !tokens with
          | { kind = Lexer.Punct Lexer.LParen; _ } :: _ ->
              let args = parse_args tokens [] in
              Some
                {
                  kind = Funcall (id_node, args);
                  span = span_from_to_node id_node (Util.last_of_list args);
                }
          | _ -> Some id_node)
      | Lexer.Number num ->
          tokens := rest;
          Some { kind = Number num; span = span_single token }
      | Lexer.StrLit str ->
          tokens := rest;
          Some { kind = StrLit str; span = span_single token }
      | Lexer.Punct Lexer.LParen -> (
          (match !tokens with
          | { kind = Lexer.Punct Lexer.LParen; _ } :: rest -> tokens := rest
          | _ -> Log.print_fatal "expected '('");
          print_endline (Lexer.tokens_to_string !tokens);
          let token = parse_expr tokens in
          match !tokens with
          | { kind = Lexer.Punct Lexer.RParen; _ } :: rest ->
              tokens := rest;
              Some token
          | _ ->
              print_ast_node_span token;
              Log.print_fatal "expected ')' after expression";
              None)
      | _ ->
          Lexer.print_token_span_no_file token;
          Log.print_fatal
            (Printf.sprintf "Unexpected token %s" (Lexer.token_to_string token));
          None)
  | _ -> None

and parse_expression_1 (tokens : Lexer.token list ref) (lhs : ast_node ref)
    min_precedence =
  while should_keep_going !tokens min_precedence do
    match !tokens with
    | { kind = Lexer.Operator op; _ } :: rest -> (
        tokens := rest;

        let rhs_maybe = parse_primary tokens in
        match rhs_maybe with
        | Some rhs_some ->
            let rhs = ref rhs_some in

            (match !tokens with
            | { kind = Lexer.Operator next_op; _ } :: _
              when Lexer.op_prec next_op > Lexer.op_prec op ->
                rhs := parse_expression_1 tokens rhs (min_precedence + 1)
            | _ -> ());
            lhs :=
              {
                kind = BinOp (!lhs, op, !rhs);
                span = span_from_to_node !lhs !rhs;
              }
        | _ -> () (* todo: leave an error message *))
    | _ -> ()
  done;
  !lhs

and parse_expr (tokens : Lexer.token list ref) =
  let primary = parse_primary tokens in
  match primary with
  | Some t -> parse_expression_1 tokens (ref t) 0
  | _ -> failwith "could not parse primary"

and parse_assignment (tokens : Lexer.token list ref) =
  match !tokens with
  | { kind = Lexer.Ident ident; _ } :: rest -> (
      tokens := rest;
      match !tokens with
      | { kind = Lexer.Punct Lexer.Assign; _ } :: rest ->
          tokens := rest;
          let value = parse_expr tokens in
          (ident, value)
      | token :: _ ->
          Lexer.print_token_span_no_file token;
          Log.print_fatal "expected '='";
          failwith "Parsing error"
      | _ -> failwith "Parsing error. TODO ERROR MESSAGE")
  | token :: _ ->
      Lexer.print_token_span_no_file token;
      Log.print_fatal "expected <ident> = <expr>";
      failwith "Parsing error"
  | _ -> failwith "Parsing error. TODO ERROR MESSAGE"

and parse_statement (tokens : Lexer.token list ref) =
  match !tokens with
  | { kind = Lexer.Keyword Lexer.Let; pos; len } :: rest -> (
      tokens := rest;
      let name, value = parse_assignment tokens in
      let stmt =
        {
          kind = LetStatment (name, value);
          span = span_from_pos_and_len pos len;
        }
      in
      match !tokens with
      | { kind = Lexer.Punct Lexer.SemiColon; _ } :: rest ->
          tokens := rest;
          stmt
      | token :: _ ->
          Lexer.print_token_span_no_file token;
          Log.print_fatal "expected ';' before here";
          let file = Util.read_whole_file stmt.span.file in
          let p = stmt.span.t in
          let len = Util.get_line_length file stmt.span.t.line in
          print_ast_span_no_file
            {
              f = { line = p.line; column = len - 1; file = p.file };
              t = { line = p.line; column = len; file = p.file };
              file = p.file;
            };
          Log.print_help "put ';' after here";
          failwith "Parsing error"
      | _ -> failwith "Parsing error. TODO ERROR MESSAGE")
  | token :: _ ->
      Lexer.print_token_span_no_file token;
      Log.print_fatal "expected statement";
      failwith "Parsing error"
  | _ -> failwith "Parsing error. TODO ERROR MESSAGE"
