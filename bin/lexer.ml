type number = Float of float | Int of int
type operator = Mul | Div | Add | Sub
type position = { line : int; column : int; file : string }
type punct = LParen | RParen | Comma

type token_kind =
  | Ident of string
  | Number of number
  | StrLit of string
  | Boolean of bool
  | Operator of operator
  | Punct of punct

type token = { kind : token_kind; pos : position; len : int }

let explode_string s = List.init (String.length s) (String.get s)

let advance_pos (pos, c) =
  match c with
  | '\n' -> { line = pos.line + 1; column = 1; file = pos.file }
  | _ -> { pos with column = pos.column + 1 }

let token_from_string string =
  match string with
  | "" -> None
  | string when String.starts_with ~prefix:"\"" string ->
      Some (StrLit (String.sub string 1 (String.length string - 2)))
  | "true" | "false" -> Some (Boolean (bool_of_string string))
  | _ -> (
      try
        let int_value = int_of_string string in
        Some (Number (Int int_value))
      with _ -> (
        try
          let float_value = float_of_string string in
          Some (Number (Float float_value))
        with _ -> Some (Ident string)))

let token_from_char c =
  match c with
  | '*' -> Some (Operator Mul)
  | '/' -> Some (Operator Div)
  | '+' -> Some (Operator Add)
  | '-' -> Some (Operator Sub)
  | '(' -> Some (Punct LParen)
  | ')' -> Some (Punct RParen)
  | ',' -> Some (Punct Comma)
  | _ -> None

let create_token (tokens, word_buffer) =
  match word_buffer with
  | "" -> tokens
  | _ -> tokens @ [ token_from_string word_buffer ]

let is_bin_op token = match token.kind with Operator _ -> true | _ -> false
let op_prec op = match op with Add -> 1 | Sub -> 1 | Mul -> 2 | Div -> 2

let precedence token =
  match token.kind with Operator op -> op_prec op | _ -> 0

let rec advance_single_comment file =
  match file with
  | '\n' :: rest -> rest
  | _ :: rest -> advance_single_comment rest
  | _ -> file

let lex file_content file_path =
  let position = { line = 1; column = 1; file = file_path } in
  let rec impl (file, tokens, word_buffer, pos, last_pos) =
    let push_token () =
      match token_from_string word_buffer with
      | None -> tokens
      | Some kind ->
          tokens @ [ { kind; pos = last_pos; len = String.length word_buffer } ]
    in
    match file with
    | [] -> push_token ()
    | '\n' :: rest ->
        impl (rest, push_token (), "", advance_pos (pos, '\n'), last_pos)
    | ' ' :: rest ->
        impl (rest, push_token (), "", advance_pos (pos, ' '), last_pos)
    | '/' :: rest -> (
        let tokens = push_token () in
        match rest with
        | '/' :: _ ->
            impl
              ( advance_single_comment rest,
                tokens,
                "",
                advance_pos (pos, '\n'),
                last_pos )
        | _ ->
            impl
              ( rest,
                tokens
                @ [ { kind = Util.unwrap (token_from_char '/'); pos; len = 1 } ],
                "",
                advance_pos (pos, ' '),
                last_pos ))
    | c :: rest when Option.is_some (token_from_char c) ->
        impl
          ( rest,
            push_token ()
            @ [ { kind = Util.unwrap (token_from_char c); pos; len = 1 } ],
            "",
            advance_pos (pos, c),
            last_pos )
    | c :: rest ->
        impl
          ( rest,
            tokens,
            word_buffer ^ String.make 1 c,
            advance_pos (pos, c),
            pos )
  in
  impl (explode_string file_content, [], "", position, position)

let operator_to_string op =
  match op with Mul -> "*" | Div -> "/" | Add -> "+" | Sub -> "-"

let punct_to_string p =
  match p with LParen -> "(" | RParen -> ")" | Comma -> ","

let num_to_string num =
  match num with
  | Float f -> Printf.sprintf "%f" f
  | Int i -> Printf.sprintf "%i" i

let token_kind_to_string t =
  match t with
  | Ident id -> id
  | Number num -> num_to_string num
  | StrLit string -> Printf.sprintf "\"%s\"" string
  | Boolean b -> if b then "true" else "false"
  | Operator op -> operator_to_string op
  | Punct p -> punct_to_string p

let token_kind_to_string_dbg t =
  match t with
  | Ident id -> Printf.sprintf "Ident(%s)" id
  | Number num -> num_to_string num
  | StrLit string -> Printf.sprintf "\"%s\"" string
  | Boolean b -> if b then "true" else "false"
  | Operator op -> Printf.sprintf "Op(%s)" (operator_to_string op)
  | Punct p -> Printf.sprintf "Punc('%s')" (punct_to_string p)

let position_to_string pos = Printf.sprintf "%i:%i" pos.line pos.column

let token_to_string_dbg t =
  Printf.sprintf "(%s, %s, %i)"
    (token_kind_to_string_dbg t.kind)
    (position_to_string t.pos) t.len

let token_to_string t = Printf.sprintf "%s" (token_kind_to_string t.kind)

let print_token_span (t, file) =
  let column_start = t.pos.column - (t.len - 1) in
  let column =
    if t.len > 1 then Printf.sprintf "%i-%i" column_start t.pos.column
    else Printf.sprintf "%i" t.pos.column
  in
  print_endline
    (Log.bold_text
       (Printf.sprintf "File: %s, line %i, column %s" t.pos.file t.pos.line
          column));
  Printf.printf "%i | %s\n" t.pos.line
    (Util.get_line_from_string t.pos.line file);
  let len = String.length (Printf.sprintf "%i | " t.pos.line) in
  let spaces = String.make (len + column_start - 1) ' ' in
  let carrots = Log.red_text (String.make t.len '^') in
  Printf.printf "%s%s\n" spaces carrots;
  ()

let print_token_span_no_file t =
  print_token_span (t, Util.read_whole_file t.pos.file)

let tokens_to_string tokens =
  List.map (function t -> token_to_string t) tokens |> String.concat ", "

let tokens_to_string_dbg tokens =
  List.map (function t -> token_to_string_dbg t) tokens |> String.concat ", "
