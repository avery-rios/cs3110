let rec print_int_list = function
  | [] -> ()
  | h :: t -> print_int h ; print_newline () ; print_int_list t