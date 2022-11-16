#use "pc.ml";;
#use "utils.ml";;
open PC;;

exception Invalid_syntax of string * char list;;
  
let one_of_words words =
  disj_list (List.map word words);;

let make_padded _pad_ _p_ =
  let _stared_pad_ = star _pad_ in
  let _padded_ = caten _stared_pad_ (caten _p_ _stared_pad_) in
  pack _padded_ cadr;;

let make_enclosed _l_ _p_ _r_ =
  pack (caten _l_ (caten _p_ _r_)) cadr ;;

let enclose_with _p_ _l_ _r_ = make_enclosed _l_ _p_ _r_;;

let parse _p_ g =
  let (m, r) = _p_ (string_to_list g) in
  if r = [] then m else raise (Invalid_syntax (g, r));;

let parse_list _p_ gs =
  List.map (parse _p_) gs;;
  
let parse_file _p_ f =
  let s = file_to_string f in
  parse _p_ s;;
