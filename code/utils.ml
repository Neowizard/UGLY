


let tab_width = 4;;                       
let car (x, _) = x;;
let cdr (_, x) = x;;
let cadr x = car (cdr x);;
let nth i = (flip List.nth) i;;

let count_if pred l =
  List.fold_left (fun s e -> if (pred e) then s + 1 else s) 0 l;;

let str_count_if pred s = count_if pred (String.to_list s);;
  
let (---) x y = List.of_enum (x--y);;
let (@@) l n = List.nth l n;;

let file_to_string file =
  let file = Pervasives.open_in file in
  let str = really_input_string file (in_channel_length file) in
  Pervasives.close_in file;
  str;;
