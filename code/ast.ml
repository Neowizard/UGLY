module AST = struct 

  type pack_param =
    | NumParam of int
    | StrParam of string
                    
  type bnf_ast =
    | Pack of string * pack_param list * bnf_ast
    | Nt of string * bnf_ast
    | List of bnf_ast list
    | Leaf of string;;

  exception Invalid_ast of bnf_ast * string;;
  exception Invalid_ast_index of int * bnf_ast;;

  let nt_name ast = match ast with
    | Nt(name, ast) -> name
    | _ -> raise (Invalid_ast (ast, "Expected Nt"));;
    
  let rec ast_len ast = match ast with
    | (Leaf(_) | Pack(_, _, _)) -> 1
    | Nt(name, ast) -> ast_len ast
    | List(asts) -> List.length asts
                                                
  let rec ast_unpack ast = match ast with
    | Pack(name, params, ast) -> ast
    | _ -> ast;;
    
  let rec ast_nth n ast =
    match ast with 
    | List(asts) when n >= (List.length asts) ->
       raise (Invalid_ast_index(n, ast))
    | (Leaf(_) | Pack(_, _, _)) when n > 0 ->
       raise (Invalid_ast_index(n, ast))
    | List(asts) -> List.nth asts n
    | Nt(name, ast) -> ast_nth n ast
    | (Leaf(_) | Pack(_, _, _))-> ast

  let rec ast_head n ast =
    match ast with
    | List(asts) -> List.take n asts
    | Nt(name, ast) -> ast_head n ast
    | (Leaf(_) | Pack(_, _, _)) -> List.take n [ast]
                                             
  let rec ast_tail n ast =
    match ast with
    | List(asts) when n >= (List.length asts)->
       []
    | List(asts) -> List.drop n asts
    | Nt(name, ast) -> ast_tail n ast
    | (Leaf(_) | Pack(_, _, _)) when n = 0 ->  [ast]
    | (Leaf(_) | Pack(_, _, _)) -> [];;

  let params_to_string params =
    let param_to_string = function
      | NumParam i -> Printf.sprintf "NumParam %d" i
      | StrParam s -> Printf.sprintf "StrParam %s" s in
    String.join "; " (List.map param_to_string params);;

    
  let rec ast_to_string ast =
    let rec to_str depth ast = match ast with 
    | Leaf str -> Printf.sprintf "%sLeaf (\"%s\")" (String.make depth ' ') str
    | Nt(name, ast) -> Printf.sprintf "%sNt (\"%s\", %s)" (String.make depth ' ') name (to_str (depth + 1) ast)
    | List(asts) -> Printf.sprintf "%sList [%s]"
                                   (String.make depth ' ')
                                   (String.join ";"
                                                (List.map (to_str (depth + 1)) asts))
    | Pack(name, params, ast) -> Printf.sprintf "%sPack (\"%s\", [%s], %s)"
                                                (String.make depth ' ')
                                                name
                                                (params_to_string params)
                                                (to_str (depth + 1) ast) in
    to_str 0 ast
end;;
