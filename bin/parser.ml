type ast_node_kind =
  | BinOp of ast_node * Lexer.operator * ast_node
  | Ident of string
  | Number of Lexer.number

and ast_node = { kind : ast_node_kind; span : span }
and span = { f : Lexer.position; t : Lexer.position }

let span_todo () =
  {
    f = { line = 0; column = 0; file = "TODO" };
    t = { line = 0; column = 0; file = "TODO" };
  }

let rec ast_to_string ast =
  match ast.kind with
  | BinOp (lhs, op, rhs) ->
      Printf.sprintf "BinOp(%s, %s, %s)" (ast_to_string lhs)
        (Lexer.operator_to_string op)
        (ast_to_string rhs)
  | Ident s -> s
  | Number num -> Lexer.num_to_string num

let parse_primary (tokens : Lexer.token list ref) =
  match !tokens with
  | token :: rest -> (
      match token.kind with
      | Lexer.Ident id ->
          tokens := rest;
          Some { kind = Ident id; span = span_todo () }
      | Lexer.Number num ->
          tokens := rest;
          Some { kind = Number num; span = span_todo () }
      | _ -> None)
  | _ -> None

(* parse_expression_1(lhs, min_precedence) *)
(*     lookahead := peek next token *)
(*     while lookahead is a binary operator whose precedence is >= min_precedence *)
(*         op := lookahead *)
(*         advance to next token *)
(*         rhs := parse_primary () *)
(*         lookahead := peek next token *)
(*         while lookahead is a binary operator whose precedence is greater *)
(*                  than op's, or a right-associative operator *)
(*                  whose precedence is equal to op's *)
(*             rhs := parse_expression_1 (rhs, precedence of op + (1 if lookahead precedence is greater, else 0)) *)
(*             lookahead := peek next token *)
(*         lhs := the result of applying op with operands lhs and rhs *)
(*     return lhs *)

let should_keep_going (tokens : Lexer.token list) min_prec =
  match tokens with
  | token :: _ -> (
      match token.kind with
      | Lexer.Operator op -> Lexer.op_prec op >= min_prec
      | _ -> false)
  | _ -> false

let rec parse_expression_1 (tokens : Lexer.token list ref) (lhs : ast_node ref)
    min_precedence =
  while should_keep_going !tokens min_precedence do
    match !tokens with
    | { kind = Lexer.Operator op; _ } :: rest ->
        tokens := rest;

        let rhs_maybe = parse_primary tokens in
        let rhs = ref (Util.unwrap rhs_maybe) in

        (match !tokens with
        | { kind = Lexer.Operator next_op; _ } :: _
          when Lexer.op_prec next_op > Lexer.op_prec op ->
            rhs := parse_expression_1 tokens rhs (min_precedence + 1)
        | _ -> ());
        lhs := { kind = BinOp (!lhs, op, !rhs); span = span_todo () }
    | _ -> ()
  done;
  !lhs

let parse_expr (tokens : Lexer.token list ref) =
  let primary = parse_primary tokens in
  match primary with
  | Some t -> parse_expression_1 tokens (ref t) 0
  | _ -> failwith "could not parse primary"

(* let rec parse_expression_1 lhs min_precedence tokens = *)
(*   let rec loop lhs = *)
(*     match tokens with *)
(*     | [] -> lhs *)
(*     | token :: _ when not (Lexer.is_binary_operator token) -> lhs *)
(*     | token :: _ -> *)
(*         let op = Lexer.get_operator token in *)
(*         if Lexer.precedence op < min_precedence then lhs *)
(*         else begin *)
(*           let next_precedence = Lexer.precedence op + if Lexer.is_right_associative op then 0 else 1 in *)
(*           let rest_tokens = Lexer.advance tokens in *)
(*           match parse_primary rest_tokens with *)
(*           | rest_tokens, Some rhs -> *)
(*               let rec parse_rhs lhs = *)
(*                 match rest_tokens with *)
(*                 | [] -> lhs *)
(*                 | lookahead :: _ when not (Lexer.is_binary_operator lookahead) -> lhs *)
(*                 | lookahead :: _ -> *)
(*                     let lookahead_op = Lexer.get_operator lookahead in *)
(*                     let lookahead_precedence = Lexer.precedence lookahead_op in *)
(*                     if lookahead_precedence > Lexer.precedence op || *)
(*                        (lookahead_precedence = Lexer.precedence op && Lexer.is_right_associative lookahead_op) then begin *)
(*                       let new_precedence = Lexer.precedence op + if lookahead_precedence > Lexer.precedence op then 1 else 0 in *)
(*                       let rhs, rest_tokens = parse_expression_1 rhs new_precedence rest_tokens in *)
(*                       parse_rhs (BinOp (lhs, op, rhs)) *)
(*                     end else lhs *)
(*               in *)
(*               let rhs = parse_rhs rhs in *)
(*               loop (BinOp (lhs, op, rhs)) *)
(*           | rest_tokens, None -> lhs (* Error handling: unable to parse primary expression *) *)
(*         end *)
(*   in *)
(*   loop lhs, tokens *)

(* let push_node node ast = *)
(*   match ast with *)
(*   | Program stmts -> Program (stmts @ [ node ]) *)
(*   | _ -> failwith "TODO" *)

(* let parse tokens ast = *)
(*   match tokens with *)
(*   | t :: rest -> ( *)
(*       match t with *)
(*       | Lexer.Number n -> *)
(*           parse rest (push_node { kind = Number n; span = span_todo () } ast) *)
(*       | _ -> parse rest ast) *)
(*   | [] -> ast *)
