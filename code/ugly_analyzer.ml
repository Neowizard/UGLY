  
module UglyAnalyzer = struct
  open UglyParser;;
  exception Invalid_expr of string * expr;;
  exception Invalid_statement_expression of statement;;

    
  let settings_names = ["line_limit"; "soft_line_limit"; "tab_width"; "whitespace";];;
  let ops_names = ["append"; "indent"; "soft_wrap"; "verbatim"; "upcase"; "lowcase";];;

  let is_nt  = function
    | Nt _ -> true | _ -> false;;
  let get_nt expr = match expr with 
    | Nt nt -> nt | _ -> raise (Invalid_expr ("expected Nt", expr))
  let is_list  = function
    | List _ -> true | _ -> false;;
  let get_list expr = match expr with 
    | List l -> l | _ -> raise (Invalid_expr ("expected List", expr))
  let is_var  = function
    | Var _ -> true | _ -> false;;
  let get_var expr = match expr with 
    | Var v -> v | _ -> raise (Invalid_expr ("expected Var", expr))
  let is_num  = function
    | Num _ -> true | _ -> false;;
  let get_num expr = match expr with  
    | Num n -> n | _ -> raise (Invalid_expr ("expected Num", expr))
  let is_lit = function
    | (Lit _ | LitCI(_)) -> true | _ -> false;;
  let get_lit expr = match expr with 
    |  (Lit s | LitCI(s)) -> s | _ -> raise (Invalid_expr ("expected Lit/LitCI", expr));;

                                            
  let verify_def = function
    | (ProdBy(s, e) | RHSOf(s,e)) -> (match e with
                                      | List(es) -> not (List.exists is_num es)
                                      | _ -> not (is_num e));;

  let verify_cond = function
    | (LT(b, v, e) | GT(b, v, e) | EQ(b, v, e) | LTE(b, v, e) | GTE(b, v, e) | NEQ(b, v, e)) -> is_num e
    | (Contains(b, v, e) | EndsWith(b, v, e) | StartsWith(b, v, e)) -> is_lit e;;

  let rec verify_statement = function
    | Setting(s, e) ->
       (List.exists (String.equal s) settings_names) && not (is_var e)
    | Op(o, es) -> (List.exists (String.equal o) ops_names)
    | Let(d) -> verify_def(d)         
    | Under(b, e, stmts) -> (match e with
                             | List(es) -> not (List.exists is_num es)
                             | _ -> not (is_num e))
    | If(c, stmts) -> verify_cond(c) && (List.for_all verify_statement stmts);;

    
  let rec exprs_to_lists stmts =         (* All expressions in Ugly are lists *)
    let pack_expr e = match e with
      | List(es) -> e
      | _ -> List [e] in
    let pack_stmt stmt = match stmt with
      | Setting(s, e) -> Setting(s, pack_expr e)
      | Op(s, es) -> Op(s, List.map pack_expr es)
      | Under(b, e, stmts) -> Under(b, pack_expr e, exprs_to_lists stmts)
      | _ -> stmt in
    List.map pack_stmt stmts;;

end;;    
    

