#use "utils.ml";;
#use "ast.ml";;
#use "styles.ml";;
#use "ugly_parser.ml";;

   
module UglyInterpreter = struct

  open UglyParser;;
  exception Not_yet_implemented of string;;
  exception Invalid_expr_in_setting of string * expr;;
  exception Invalid_statement of statement';;
  type doc_layout = {line_limit: int; tab_width: int; soft_line_limit: int; whitespaces:char list};;
  type doc =
    | Text of doc_layout * string
    | Doc of doc_layout * doc list;;

  let default_layout = {line_limit = 80; soft_line_limit = 80; tab_width = 4; whitespaces = [' '; '\n'; '\t']};;
    
  let apply_setting layout str e = 
    let fs = [("line_limit",
               function | Num(i) -> {layout with line_limit=i}
               | _ -> raise (Invalid_expr_in_setting (str, e)));
              ("tab_width",
               function Num(i) -> {layout with tab_width=i}
                      | _ -> raise (Invalid_expr_in_setting (str, e)));
              ("soft_line_limit",
               function | Num(i) -> {layout with soft_line_limit=i}
               | _ -> raise (Invalid_expr_in_setting (str, e)));
              ("whitespaces",
               fun _ -> raise (Not_yet_implemented "whitespaces setting"))] in
    (List.assoc str fs) e;;

  let add_var vars def =
    match def with
    | RHSOf'(v, expr) -> raise (Invalid_statement e)
    | ProdBy'(v, expr) ->

  let rec ast_to_doc ugls pred vars layout ast =
    match ugls with
    | Setting'(str, e)::ugls -> ast_to_doc ugls pred vars (apply_setting layout str e) ast
    | Op'(str, es)::ugls -> raise (Not_yet_implemented "Op to doc")
    | Let'(def)::ugls -> ast_to_doc ugls pred (add_var vars def) layout ast
    | Under'(b, e, block)::ugls -> Doc
      
  (* let rec verbatim pred = *)
  (*   fun ast -> *)
  (*   if (pred ast) then (Leaf (ast_verb ast)) *)
  (*   else match ast with *)
  (*              | Leaf(s, verb) -> ast *)
  (*              | (Caten(asts, verb) | List(asts, verb)) -> List(List.map (verbatim_nt nt) asts, verb) *)
  (*              | Pack(s, ast, verb) -> Pack(s, verbatim_nt nt ast, verb) *)
  (*              | Nt(n, ast, verb) where nt=n -> Leaf verb *)
  (*              | Nt(n, ast, verb) -> Nt(n, verbatim_nt nt ast, verb);; *)

(*    
  and append pred str =
    let str = Nt("{append}", Leaf str) in
    fun layout ast ->
    if (pred ast) then Doc(layout, ast_to_box (List [ast; str]))
    else match ast with 
         | Leaf(s) -> Text(layout, ast_to_doc layout ast)
         | (Caten(asts) | List(asts)) -> Doc(layout, List.map ast_to_doc pred str asts) 
         | Pack(s, p, ast) -> Pack(s, p, append pred str) 
         | Nt(n, ast) -> Nt(n, append pred str);; 
 *)
    
    
        
end;;
