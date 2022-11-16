#use "utils.ml";;
#use "ast.ml";;
#use "box.ml";;
#use "runtime.ml";;
#use "ugly_parser.ml";;
#use "ugly_analyzer.ml";;
#use "bnf_parser.ml";;
#use "bnf_compiler.ml";;
#use "ugly_compiler.ml";;
open UglyParser;;
open UglyCompiler;;    
open AST;;


let sub_box_print b =
  List.map print_box b#sub_boxes;;
let sub b i = List.nth b#sub_boxes i;;

let rec sub_get b is =
  match is with
  | [] -> b
  | i::is -> sub_get (List.nth b#sub_boxes i) is;;
  

let test_ugl ugl bnf in_file =
  ast_to_box (beautify_f ugl bnf in_file)
  
let test s =
  let ugl = Printf.sprintf "%s.ugl" s in
  let bnf = Printf.sprintf "%s.bnf" s in
  let in_file = Printf.sprintf "%s.in" s in
  print_box (test_ugl ugl bnf in_file)

let test_frag f str nt =
  let ugl = Printf.sprintf "%s.ugl" f in
  let bnf = Printf.sprintf "%s.bnf" f in
  beautify (file_to_string ugl) (file_to_string bnf) str nt;;

let p = "write";;
let a () = test_frag "pascal" p "WRITE_PREFIX";;
let abox = ast_to_box (a ());;
print_box abox

let b = horz_bot [horz_bot [string_to_box "'ca'";
                             string_to_box ")";];
                   string_to_box ";"]
    
