let unwrap o =
  match o with Some v -> v | _ -> failwith "ERROR: unwrapping None value"

let get_line_from_string line_number text =
  let lines = String.split_on_char '\n' text in
  match List.nth_opt lines (line_number - 1) with
  | Some line -> line
  | None ->
      print_endline "Line number out of range";
      ""

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let rec last_of_list l =
  match l with
  | [] -> failwith "Empty list"
  | [ x ] -> x (* Base case: single element in the list *)
  | _ :: rest ->
      last_of_list
        rest (* Recursive case: discard the first element and continue *)

let get_line_length file_content line_number =
  let lines = String.split_on_char '\n' file_content in
  match List.nth_opt lines (line_number - 1) with
  | Some line -> String.length line
  | None -> failwith "Line number out of range"
