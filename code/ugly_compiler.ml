

module UglyCompiler = struct
  open UglyParser;;
  exception Not_yet_implemented of string;;
  exception Invalid_expr of string * expr;;
  exception Invalid_statement of statement;;
  exception Op_not_found of string * string list;;
  type doc_layout = {line_limit: int; tab_width: int; soft_line_limit: int; whitespaces:char list};;
  type doc =
    | Text of doc_layout * string
    | Doc of doc_layout * doc list;;

  let default_layout = {line_limit = -1; soft_line_limit = 80; tab_width = 4; whitespaces = [' '; '\n'; '\t']};;
 
  let apply_setting layout str e = 
    let fs = [("line_limit",
               function | Num(i) -> {layout with line_limit=i}
               | _ -> raise (Invalid_expr (str, e)));
              ("tab_width",
               function Num(i) -> {layout with tab_width=i}
                      | _ -> raise (Invalid_expr (str, e)));
              ("soft_line_limit",
               function | Num(i) -> {layout with soft_line_limit=i}
               | _ -> raise (Invalid_expr (str, e)));
              ("whitespaces",
               fun _ -> raise (Not_yet_implemented "whitespaces setting"))] in
    (List.assoc str fs) e;;

(*  let add_var vars def =
    match def with
    | RHSOf'(v, expr) -> raise (Invalid_statement e)
    | ProdBy'(v, expr) -> raise (Not_yet_implemented "variables");;
 *)

    

  let rec ugly_expr_to_string e =
    match e with
    | Num i -> Printf.sprintf "Num(%d)" i
    | Lit s -> Printf.sprintf "Lit(%s)" s
    | LitCI s -> Printf.sprintf "LitCI(%s)" s
    | Nt s -> Printf.sprintf "Nt(%s)" s
    | List l ->
       Printf.sprintf "List [%s]"
                      (String.join ", "
                                   (List.map ugly_expr_to_string l))

                      
  let add_under_pred preds neg expr =
    let rec expr_to_pred neg e = 
        match e with
        | Nt s ->
           (fun e ->
             match e with
             | AST.Nt (str,_) ->
                (s = str) == neg
             | _ ->
                false)
        | (Lit s | LitCI s) ->
           (fun e ->
             match e with
             | AST.Leaf str -> (s = str) == neg
             | _ -> false)
        | List es ->
           let preds = List.map (expr_to_pred false) es in
           fun ast ->
           List.exists (fun p -> (p ast) == neg) preds
        | _ -> raise (Invalid_expr ("Under", expr)) in
    let pred = expr_to_pred neg expr in
    preds@[pred];;
    
  open UglyAnalyzer;;

  let apply_transformations ts ast =
    List.fold_right (fun t ast -> t ast) ts ast;;
    
  open AST;;

  let rec test_preds preds ast =
    match preds with
    | [] -> []
    | pred::rest ->
           if (pred ast)
           then test_preds rest ast
           else preds

  let op_line_wrap name preds params layout =
    if (layout.line_limit < 0) then (fun ast -> ast)
    else
      fun ast -> Pack("line-wrap", [NumParam (layout.line_limit); NumParam (layout.tab_width)], ast)
    
  let pack_nt pack pack_params name preds params layout =
    let apply_to = get_nt (List.nth params 0) in
    let rec op preds ast =
      let preds = test_preds preds ast in
      let apply_op = preds == [] in
      match ast with
      | Leaf _ -> ast
      | List asts -> List (List.map (fun ast -> op preds ast) asts)
      | Nt (nt, ast) when (nt = apply_to) && apply_op ->
         Pack(pack, pack_params, Nt(nt, op preds ast))
      | Nt (nt, ast) ->
         Nt(nt, op preds ast)
      | Pack(n, p, ast) -> Pack(n, p, op preds ast) in
    op preds

           
  let pack_nts pack pack_params name preds params layout =
    let nts = get_list (List.nth params 0) in
    let ts = List.map (fun nt -> pack_nt "vertical" pack_params "pack_nts" preds [nt] layout) nts in
    fun ast -> apply_transformations ts ast;;
    
    (*let apply_to = get_nt (List.nth params 0) in
    let rec op preds ast =
      let preds = test_preds preds ast in
      let apply_op = preds == [] in
      match ast with
      | Leaf _ -> ast
      | List asts -> List (List.map op asts)
      | Nt (nt, ast) when (nt = apply_to) && apply_op ->
         Pack("vertical", [], Nt(nt, op preds ast))
      | Nt (nt, ast) ->
         Nt(nt, op preds ast)
      | Pack(n, p, ast) -> Pack(n, p, op preds ast) in
    op preds;;
     *)


  let op_replace name preds params layout =
    let pack_param = StrParam (get_lit (List.nth params 1)) in
    pack_nt "replace" [pack_param] name preds params layout
                             
       
  let op_prepend name preds params layout =
    let pack_param = StrParam (get_lit (List.nth params 1)) in
    pack_nt "prepend" [pack_param] name preds params layout
            
  let op_append name preds params layout =
    let pack_param = StrParam (get_lit (List.nth params 1)) in
    pack_nt "append" [pack_param] name preds params layout
    (*
    let apply_to = get_nt (List.nth params 0) in
    let rec op preds ast =
      let preds = test_preds preds ast in
      let apply_op = preds == [] in
      match ast with
      | Leaf _ -> ast
      | List asts -> List (List.map (fun ast -> op preds ast) asts)
      | Nt (nt, ast) when (nt = apply_to) && apply_op ->
         Pack("append", [StrParam str], Nt(nt, op preds ast))
      | Nt (nt, ast) ->
         Nt(nt, op preds ast)
      | Pack(n, p, ast) -> Pack(n, p, op preds ast) in
    op preds;;
     *)

  let op_indent name preds params layout =
    let pack_param = NumParam (layout.tab_width) in
    pack_nt "indent" [pack_param] name preds params layout

            
  let decorate_leaves f name preds params layout =
    let apply_to = get_nt (List.nth params 0) in
    let nt_pred = (function
                    | Nt (str,_) -> (apply_to = str)
                    | _ -> false) in
    let preds = nt_pred::preds in
    let rec op preds ast =
      let preds = test_preds preds ast in
      let apply_op = preds == [] in
      match ast with
      | Leaf s when apply_op -> f ast
      | Leaf s -> ast
      | List asts -> List (List.map (op preds) asts)
      | Nt(nt, ast) -> Nt(nt, op preds ast)
      | Pack(n, p, ast) -> Pack(n, p, op preds ast) in
    op preds
         
  let op_upcase =
    let f ast = match ast with 
      | Leaf s -> Pack("upcase", [], ast)
      | _ -> raise (Invalid_ast(ast, "op_upcase: expected a leaf")) in
    decorate_leaves f;;

  let op_downcase =
    let f ast = match ast with 
      | Leaf s -> Pack("downcase", [], ast)
      | _ -> raise (Invalid_ast(ast, "op_downcase: Expected a leaf")) in
    decorate_leaves f;;

  let op_wrap name preds params layout =
    let apply_to = get_nt (List.nth params 0) in
    let rec find_nt asts name idx = match asts with
      | Nt(nt, ast)::asts when (nt = name) -> idx
      | ast::asts -> find_nt asts name (idx+1)
      | [] -> -1 in
    let rec op preds ast = 
      let preds = test_preds preds ast in
      let apply_op = preds == [] in
      match ast with
      | Leaf _ -> ast
      | List asts when apply_op ->
         let idx = find_nt asts apply_to 0 in
         if (idx >= 0)
         then Pack("wrap", [NumParam (idx+1)], List (List.map (op preds) asts))
         else List (List.map (op preds) asts)
      | List asts -> List (List.map (op preds) asts)
      | Nt(nt, ast) -> Nt(nt, op preds ast)
      | Pack(n, p, ast) -> Pack(n, p, op preds ast) in
    op preds;;

    
  let op_skip_wrap name preds params layout =
    let apply_to = get_nt (List.nth params 0) in
    let rec find_nt asts name idx = match asts with
      | Nt(nt, ast)::asts when (nt = name) -> idx
      | ast::asts -> find_nt asts name (idx+1)
      | [] -> -1 in
    let rec op preds ast = 
      let preds = test_preds preds ast in
      let apply_op = preds == [] in
      match ast with
      | Leaf _ -> ast
      | List asts when apply_op ->
         let idx = find_nt asts apply_to 0 in
         if (idx >= 0)
         then Pack("skip-wrap", [NumParam idx], List (List.map (op preds) asts))
         else List (List.map (op preds) asts)
      | List asts -> List (List.map (op preds) asts)
      | Nt(nt, ast) -> Nt(nt, op preds ast)
      | Pack(n, p, ast) -> Pack(n, p, op preds ast) in
    op preds;;

  let op_vert_insert = pack_nt "vert-insert" []

  let rec compile_ugly ugls preds layout =
    match ugls with
    | If(cond, block)::ugls ->  raise (Not_yet_implemented "if")
    | Let(def)::ugls -> raise (Not_yet_implemented "variables")
    | Setting(str, e)::ugls -> compile_ugly ugls preds (apply_setting layout str e)
    | Under(neg, e, block)::ugls ->
       let under = compile_under block preds neg e layout in
       under::(compile_ugly ugls preds layout)  
    | Op(name, exprs)::ugls ->
       let op = compile_op name preds exprs layout in
       op::(compile_ugly ugls preds layout)
    | [] -> [] 
              
  and compile_ugly_file f =
    let u = parse_file _ugly_ f in
    compile_ugly u [] default_layout
         
  and compile_under ugls preds neg expr layout  =
    let preds = add_under_pred preds neg expr in
    let trans = compile_ugly ugls preds layout in
    fun ast -> apply_transformations trans ast
                               
  and compile_op name preds exprs layout =
    let ops = [
        ("append", op_append);
        ("prepend", op_prepend);
        ("vert", pack_nt "vertical" []);
        ("indent", op_indent);
        ("vert_and_insert", op_vert_insert);
        ("vert_skip_last", op_vert_insert);
        ("wrap_after", op_wrap);
        ("upcase", op_upcase);
        ("no_wrap", pack_nt "no-wrap" []);
        ("line_wrap", op_line_wrap);
        ("squeeze", pack_nt "squeeze" []);
        ("skip", (pack_nt "skip" []));
        ("vert_list", pack_nt "vert-list" []);
        ("skip_wrap", op_skip_wrap);
        ("align_center", (pack_nt "align-to-second" []));
        ("center", (pack_nt "center-off-second" []));
        ("replace", op_replace);
        ("vert_space", (pack_nt "vert-space" []));
        ("verts", pack_nts "verticals" []);
        ("verbatim", fun name preds params layout -> raise (Not_yet_implemented "op"));
        ("line_limit", fun name preds params layout -> raise (Not_yet_implemented "op"));
        ("indent", fun name preds params layout -> raise (Not_yet_implemented "op"));
        ("soft_wrap", fun name preds params layout -> raise (Not_yet_implemented "op"));
        ("prepend", fun name preds params layout -> raise (Not_yet_implemented "op"));
        ("align_centers", fun name preds params layout -> raise (Not_yet_implemented "op"));
        ("single_line", fun name preds params layout -> raise (Not_yet_implemented "op"));
        ("center", fun name preds params layout -> raise (Not_yet_implemented "op"));
        ("lowcase", fun name preds params layout -> raise (Not_yet_implemented "op"));
      ] in
  try (List.assoc name ops) name preds exprs layout
  with Not_found -> raise (Op_not_found(name, (List.map Tuple2.first ops)))

  let beautify ugl bnf input entry =
    let _bnf_ = BNFCompiler.compile_bnf bnf entry in
    let ugls = parse _ugly_ ugl in
    let ugls = ugls@[Op("line_wrap", [])] in
    let trans = compile_ugly ugls [] default_layout in
    let ast = parse _bnf_ input in
    apply_transformations trans ast

  let beautify_f ugl_f bnf_f input_f =
    beautify (file_to_string ugl_f) (file_to_string bnf_f) (file_to_string input_f) "S";;

  
end;;
