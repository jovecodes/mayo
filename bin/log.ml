let red_text text = Printf.sprintf "\027[31m%s\027[0m" text
let green_text text = Printf.sprintf "\027[32m%s\027[0m" text
let magenta_text text = Printf.sprintf "\027[35m%s\027[0m" text
let bold_text text = Printf.sprintf "\027[1m%s\027[0m" text

let print_error message =
  print_endline (bold_text (red_text "Error: ") ^ message)

let print_fatal message =
  print_endline (bold_text (red_text "Fatal: ") ^ message)

let print_help message =
  print_endline (bold_text (green_text "Help: ") ^ message)

let print_warn message =
  print_endline (bold_text (magenta_text "Warning: ") ^ message)
