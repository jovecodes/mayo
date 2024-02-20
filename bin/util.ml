let unwrap o =
  match o with Some v -> v | _ -> failwith "ERROR: unwrapping None value"
