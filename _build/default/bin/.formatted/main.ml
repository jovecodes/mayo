(* type number = *)
(*     Float of float | *)
(*     Int of int *)

type token = 
    Ident of string
    (* Number of string *)


let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s


let explode_string s = List.init (String.length s) (String.get s)


let token_from_string string =
  match string with
  | "true" | "false" -> Boolean (bool_of_string string)
  | "0b" :: rest -> Binary (int_of_string ("0b" ^ rest))
  | "0o" :: rest -> Octal (int_of_string ("0o" ^ rest))
  | "0x" :: rest -> Hexadecimal (int_of_string ("0x" ^ rest))
  | _ ->
    (try 
        let int_value = int_of_string string in
        Number (Int int_value)
    with
      _ -> (try let float_value = float_of_string string in Number (Float float_value) with _ -> Ident string))


let create_token (tokens, word_buffer) =
  match word_buffer with
  | "" -> tokens
  | _ -> tokens @ [Ident word_buffer]


let lex file_content = 
    let rec impl (file, tokens, word_buffer) =
      match file with
      | [] -> create_token (tokens, word_buffer) (* If the file is empty, create a token from the word buffer and return tokens *)
      | ' ' :: rest | '\n' :: rest ->
        let new_tokens = create_token (tokens, word_buffer) in
        impl (rest, new_tokens, "") (* If space or newline encountered, create token from word buffer and continue lexing *)
      | c :: rest -> impl (rest, tokens, word_buffer ^ (String.make 1 c)) in (* Append character to word buffer *)

    impl (explode_string file_content, [], "")


let token_to_string t = 
    match t with
    | Ident id -> id
    (* | Number num -> num *)


let () =
    let file_content = read_whole_file "./test.txt" in
    let tokens = lex file_content in
    let tokens_str = List.map (function t -> token_to_string t) tokens |> String.concat " : " in
    print_endline tokens_str
