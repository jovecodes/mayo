let unwrap o =
  match o with Some v -> v | _ -> failwith "ERROR: unwrapping None value"

let get_line_from_string line_number text =
  let lines = String.split_on_char '\n' text in
  match List.nth_opt lines (line_number - 1) with
  | Some line -> line
  | None ->
      print_endline "Line number out of range";
      ""
