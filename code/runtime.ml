open AST;;


exception Invalid_param of string * pack_param;;
exception Invalid_ast_for_style of string * bnf_ast;;
exception Invalid_params_for_style of string * pack_param list;;
exception Style_not_found of string * string list;;
  

let get_str_param params idx name =
  let param = List.nth params idx in 
  match param with 
  | StrParam s -> s
  | NumParam _ -> raise (Invalid_param (Printf.sprintf "%s: str was expected" name, param));;

let get_num_param params idx name =
  let param = List.nth params idx in
  match param with 
  | NumParam n -> n
  | StrParam _ -> raise (Invalid_param (Printf.sprintf "%s: num was expected" name, param));;

let rec ast_to_box ast : box  =
  match ast with
  | Pack(name, params, ast) -> build_style name params ast
  | Nt(name, ast) -> (ast_to_box ast)#tag (Printf.sprintf "Nt(%s)" name)
  | List asts ->
     let bs = List.map ast_to_box asts in
     let box_groups = List.fold_right
                        (fun b groups ->
                          match b#is_vert with
                          | true -> [b]::groups
                          | false ->  match groups with
                                      | [] -> [[b]]
                                      | group::groups ->
                                         (b::group)::groups) 
                        bs
                        [] in
     let groups = List.map
                    (fun g ->
                      let height = List.max (List.map (fun b -> b#height) g) in 
                      if (height > 1)
                      then insert_boxes g
                      else horz_bot g) box_groups in
     insert_boxes groups
  | Leaf(s) -> string_to_box s

and build_style style params ast =
  let styles = [("align-to-second", align_to_second);
                ("center-off-second", center_off_second);
                ("vertical", vertical);
                ("vert-insert", vert_insert);
                ("upcase", to_upper_case);
                ("downcase", to_lower_case);
                ("indent", indent);
                ("spaced", spaced);
                ("line-wrap", line_wrap);
                ("append", append);
                ("squeeze", squeeze);
                ("require-space", require_spaces);
                ("single-line", single_line);
                ("wrap-space-second", wrap_second);
                ("wrap-indent-second", wrap_second);
                ("noop", noop);
                ("no-wrap", no_wrap);
                ("skip", skip);
                ("vert-list", vert_list);
                ("replace", replace);
                ("vert-space", vert_space);
                (* ("skip-wrap", skip_wrap); *)
                ("wrap-second", wrap_second);] in
  try (List.assoc style styles) params ast style
  with Not_found -> raise (Style_not_found(style, (List.map Tuple2.first styles)))

and skip params ast name =
  empty_box
    
and no_wrap params ast name =
  let b = ast_to_box ast in
  (b#tag "no_wrap")
                          
and noop params ast name =
    ast_to_box ast

and vert_space params ast name =
  let space = string_to_box " " in
  let box = ast_to_box ast in
  vert_left [box;space] 
 
               
and append params ast name =
  let str = string_to_box (get_str_param params 0 name) in
  let box = ast_to_box ast in
  insert_bottom_right box str
  (* let rec append_list = function *)
  (*   | [] -> [str_leaf] *)
  (*   | ast::[] -> [append_str ast]  *)
  (*   | ast::asts -> ast::(append_list asts) *)
  (* and append_str ast =  *)
  (*   match ast with *)
  (*   | Leaf _ -> (List [ast; str_leaf]) *)
  (*   | Nt (name, ast) -> Nt (name, append_str ast) *)
  (*   | Pack (name, p, ast) -> Pack (name, p, append_str ast)  *)
  (*   | List asts -> List (append_list asts) in *)
  (* ast_to_box (append_str ast) *)

and prepend params ast name =
  let str_leaf = Leaf (get_str_param params 0 name) in
  let rec prepend ast = 
    match ast with
    | Leaf _ -> (List [ast; str_leaf])
    | Nt (name, ast) -> Nt (name, prepend ast)
    | Pack (name, p, ast) -> Pack (name, p, prepend ast) 
    | List (ast::asts) -> List (prepend ast::asts)
    | _ -> raise (Invalid_ast_for_style (name, ast)) in
  ast_to_box (prepend ast)

             
and replace params ast name =
  let str = get_str_param params 0 name in
  string_to_box str 

             
and indent_box tab_width b =
  let indent = string_to_box (String.make tab_width ' ') in
  horz_top [indent; b]


and break_line line_len bs =
  let rec break_line len bs lines line =
    match bs with
    | [] -> lines@[line]
    | b::bs ->
       if (b#width <= len)
       then break_line (len-b#width) bs lines (line@[b])
       else break_line (line_len-b#width) bs (lines@[line]) [b]
  in
  break_line line_len bs [] [] 
             
and wrap_box line_len tab_width b =
  if (b#has_tag "no_wrap" || b#width <= line_len) then b
  else
    let wrap_horz ctor bs =
      let bs = List.map (wrap_box line_len tab_width) bs in
      let lines = break_line line_len bs in
      let head = insert_boxes (List.nth lines 0) in
      let rest = List.drop 1 lines in
      let rest =
        List.map ctor rest
      in
      List.fold_left (fun wrapped b ->
          let inserted = insert_bottom_right wrapped b in
          let b = if (head#width == 0) then b
                  else indent_box tab_width b
          in
          if (inserted#width <= wrapped#width)
          then inserted
          else vert_left [wrapped;b])
                     head
                     rest
    in
    match b#box_struct with
    | Simple  -> b
    | VertLeft(bs) -> vert_left (List.map (wrap_box line_len tab_width) bs)
    | VertCenter(bs) -> vert_center (List.map (wrap_box line_len tab_width) bs)
    | VertRight(bs) -> vert_right (List.map (wrap_box line_len tab_width) bs)
    | HorzTop bs  -> wrap_horz horz_top bs
    | HorzMid bs  -> wrap_horz horz_mid bs
    | HorzBot bs  -> wrap_horz horz_bot bs


and line_wrap params ast name =
  let line_len = get_num_param params 0 name in
  let tab_width = get_num_param params 1 name in
  let b = ast_to_box ast in
  wrap_box line_len tab_width b
                                    
and line_len p a n =
  raise (Style_not_found ("line_len", []))
       

and align_to_second params ast name =
  match ast with
  | ((Pack _) | (Leaf _)) -> ast_to_box ast
  | Nt(name, ast) -> align_to_second params ast name
  | List(asts) ->
     let firsts = List.map (fun ast -> ast_to_box (ast_nth 0 ast)) asts in
     let max_width = List.fold_left max 0 (List.map (fun b -> b#width) firsts) in
     let firsts = List.map (fun b ->
                      let pad = string_to_box (String.make (max_width - b#width) ' ') in
                      horz_bot [pad;b]) firsts in
     let seconds = List.map (fun ast -> if (ast_len ast) > 1
                                        then ast_to_box (ast_nth 1 ast)
                                        else empty_box) asts in
     let tails = List.map (ast_tail 2) asts in
     let tails = List.map (List.map ast_to_box) tails in
     let tails = List.map horz_bot tails in
     let heads = List.map2 Tuple2.make firsts seconds in
     vert_left (List.map2 (fun (f,s) t -> horz_bot [f;s;string_to_box " ";t]) heads tails)

and center_off_second params ast name =
  match ast with
  | ((Pack _) | (Leaf _)) -> ast_to_box ast
  | Nt(name, ast) -> align_to_second params ast name
  | List(asts) ->
     let firsts = List.map (fun ast -> ast_to_box (ast_nth 0 ast)) asts in
     let max_width = List.fold_left max 0 (List.map (fun b -> b#width) firsts) in
     let firsts = List.map (fun b ->
                      let pad = string_to_box (String.make (max_width - b#width) ' ') in
                      horz_bot [b;pad]) firsts in
     let seconds = List.map (fun ast -> if (ast_len ast) > 1
                                        then ast_to_box (ast_nth 1 ast)
                                        else empty_box)
                            asts in
     let tails = List.map (ast_tail 2) asts in
     let tails = List.map (List.map ast_to_box) tails in
     let tails = List.map horz_bot tails in
     let heads = List.map2 Tuple2.make firsts seconds in
     vert_left (List.map2 (fun (f,s) t -> horz_bot [f;s;t]) heads tails)

and vert_list params ast name = 
  match ast with
  | (Leaf _) | (Pack _) -> ast_to_box ast
  | Nt(name, ast) -> vert_list params ast name
  | List(asts) ->
     if (List.length asts = 1)
     then vert_list params (List.first asts) name
     else
       let first, asts = List.split_at 1 asts in
       let asts, last = List.split_at ((List.length asts) - 1) asts in
       let bf = ast_to_box (List.first first) in
       let bl = ast_to_box (List.first last) in
       let bs = List.map (fun ast -> vert_list params ast name) asts in
       let bs = vert_left bs in
       let b = horz_top [bf; bs] in
       insert_bottom_right b bl
                               
and vert_insert params ast name =
  match ast with
  | ((Pack _) | (Leaf _) | (List [])) -> ast_to_box ast
  | Nt(name, ast) -> vert_insert params ast name
  | List(first::last::[]) ->
     insert_boxes [ast_to_box first; ast_to_box last]
  | List(ast::asts) ->
     let b1 = ast_to_box ast in
     let b2 = vert_insert params (List asts) name in
     vert_left [b1; b2]
 
               
and vertical params ast name =
   match ast with
   | ((Pack _) | (Leaf _)) -> ast_to_box ast
   | Nt(name, ast) -> vertical params ast name
   | List(asts) ->
      vert_left (List.map ast_to_box asts)

and spaced params ast name =
  let b = ast_to_box ast in
  let s = string_to_box " " in
  horz_mid [s;b;s]

and squeeze params ast name =
  match ast with
  | ((Pack _) | (Leaf _)) -> ast_to_box ast
  | Nt(name, ast) -> squeeze params ast name
  | List(ast::_) -> ast_to_box ast
  | List [] -> empty_box

and single_line params ast name =
  let b = ast_to_box ast in
  string_to_box (b#to_string_with_delim " ")

                
and wrap_second params ast name =
  let indent_width = List.nth params 0 in
  let indent_width = match indent_width with
    | StrParam(_) -> raise (Invalid_params_for_style (name, params))
    | NumParam(i) -> i in
  let pad = string_to_box (String.make indent_width ' ') in
  let first = if (ast_len ast) > 0
              then ast_to_box (ast_nth 0 ast)
              else empty_box in
  let second = if (ast_len ast) > 1
               then ast_nth 1 ast
               else Leaf("") in
  let second = match second with
    | List(asts) ->
       vert_left (List.map ast_to_box asts)
    | Nt(name, ast) -> ast_to_box ast
    | (Leaf(_) | Pack(_, _, _)) -> ast_to_box second in
  let second = append_boxes [pad;second] in
  vert_left [first; second]

and require_spaces params ast name =
  let b = ast_to_box ast in
  let c = b#get 0 (b#width - 1) in
  let spaces = [' '; '\t'] in
  if (List.exists (Char.equal c) spaces) then b
  else (horz_bot [b;(string_to_box " ")])

and to_upper_case params ast name =
  let b = ast_to_box ast in
  b#decorate Char.uppercase
  
and to_lower_case params ast name =
  let b = ast_to_box ast in
  b#decorate Char.lowercase

and indent params ast name =
  let width = List.nth params 0 in
  let width = match width with
    | StrParam(_) -> raise (Invalid_params_for_style (name, params))
    | NumParam(i) -> i in
  let b = ast_to_box ast in
  let indent = string_to_box (String.make width ' ') in
  let indent = indent#tag "indent" in
  horz_top [indent;b]
