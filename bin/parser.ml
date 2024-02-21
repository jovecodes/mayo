type ast_node_kind =
  | BinOp of ast_node * Lexer.operator * ast_node
  | Ident of string
  | Number of Lexer.number

and ast_node = { kind : ast_node_kind; span : span }
and span = { f : Lexer.position; t : Lexer.position; file : string }

let span_from_to_token (from_t : Lexer.token) (to_t : Lexer.token) =
  {
    f = { from_t.pos with column = from_t.pos.column - from_t.len };
    t = to_t.pos;
    file = from_t.pos.file;
  }

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
  | BinOp (lhs, op, rhs) ->
      Printf.sprintf "BinOp(%s, %s, %s)" (ast_to_string lhs)
        (Lexer.operator_to_string op)
        (ast_to_string rhs)
  | Ident s -> s
  | Number num -> Lexer.num_to_string num

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

let print_ast_node_span n =
  print_ast_span n.span (Util.read_whole_file n.span.file)

let rec parse_primary (tokens : Lexer.token list ref) =
  match !tokens with
  | token :: rest -> (
      match token.kind with
      | Lexer.Ident id ->
          tokens := rest;
          Some { kind = Ident id; span = span_single token }
      | Lexer.Number num ->
          tokens := rest;
          Some { kind = Number num; span = span_single token }
      | Lexer.Punct Lexer.LParen -> (
          (match !tokens with
          | { kind = Lexer.Punct Lexer.LParen; _ } :: rest -> tokens := rest
          | _ -> Log.print_fatal "expected '('");
          print_endline (Lexer.tokens_to_string !tokens);
          let token = parse_expr tokens in
          match !tokens with
          | { kind = Lexer.Punct Lexer.RParen; _ } :: rest ->
              tokens := rest;
              token
          | _ ->
              print_ast_node_span (Util.unwrap token);
              Log.print_fatal "expected ')' after expression";
              None)
      | _ ->
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
  | Some t -> Some (parse_expression_1 tokens (ref t) 0)
  | _ -> failwith "could not parse primary"
