#rectypes;;

module BNFCompiler = struct
  exception Nt_not_found of string * string list;;
  open BNFParser;;
  open AST;;
  let gen_fail expr =
    fun dict -> nt_none;;

  let make_gen_lit ctor w =
    let parser = pack (ctor w) (fun w -> Leaf(String.of_list w)) in
    fun dict -> parser;;

  let gen_lit_ci = make_gen_lit word_ci;;
    
  let gen_lit = make_gen_lit word;;
    
  let gen_range f t =
    let parser = pack (range f t) (fun r -> Leaf(String.make 1 r)) in
    fun dict  -> parser ;;
    
  let rec gen_star e =
    let parser = (gen_rhs e) in
    fun dict -> pack (star (parser dict)) (fun es -> List es)
                     
  and gen_plus e =
    let parser = (gen_rhs e) in
    fun dict  -> pack (plus (parser dict)) (fun es -> List es)
                      
  and gen_maybe e =
    let parser = (gen_rhs e) in
    fun dict  -> pack (maybe (parser dict)) (fun e ->
                        match e with
                        | Some(e) -> e
                        | None -> Leaf "")
                      

  and gen_skip e =
    let parser = gen_rhs e in
    fun dict -> pack (parser dict) (fun x -> Leaf "")

  and gen_trace e =
    let parser = gen_rhs e in
    fun dict -> trace_pc (expr_to_string e) (parser dict)
                         
  and gen_diff m s =
    let m = (gen_rhs m) in
    let s = (gen_rhs s) in
    fun dict -> (diff (m dict) (s dict)) 
                  
  and gen_caten es =
    let parsers = List.map gen_rhs es in
    fun dict -> pack (caten_list (List.map (fun p -> (p dict)) parsers))
                     (fun es -> List es)
                     
  and gen_disj es =
    let parsers = List.map gen_rhs es in
    fun dict -> disj_list (List.map (fun p -> (p dict)) parsers)

  and gen_expr cb expr =
    let parser = gen_rhs expr in
    fun dict  -> pack (parser dict)  (fun x ->
                        match cb with
                        | Default -> x
                        | Callback(name) -> Pack(name, [], x))

  and gen_nt nt =
    fun dict ->
    let thunk = fun () ->
      try ((List.assoc nt dict) dict)
      with Not_found -> raise (Nt_not_found (nt, List.map Tuple2.first dict)) in
    pack (delayed thunk) (fun n -> Nt(nt, n))
         
  and gen_rhs (e:expr) : 'a -> char list -> bnf_ast * char list =
    match e with 
    | Range(f, t) -> gen_range f t
    | Lit(s) -> gen_lit s
    | LitCi(s) -> gen_lit_ci s
    | Star(e) -> gen_star e
    | Plus(e) -> gen_plus e
    | Maybe(e) -> gen_maybe e
    | Skip(e) -> gen_skip e
    | Trace(e) -> gen_trace e
    | Diff(m, s) -> gen_diff m s
    | Caten(es) -> gen_caten es
    | Disj(es) -> gen_disj es
    | Expr(cb, expr) -> gen_expr cb expr
    | Nt(nt) -> gen_nt nt;;
    
  let add_prod dict (Prod(nt, e)) =
    (nt, gen_rhs e)::dict;;

  let framework () =
    [("tab", fun dict -> pack (char '\t') (fun _ -> Leaf "\t"));
     ("newline", fun dict -> pack (char '\n') (fun _-> Leaf "\n"));
     ("epsilon", fun dict -> pack nt_epsilon (fun _ -> Leaf ""));
     ("end_of_input", fun dict -> pack nt_end_of_input (fun _ -> Leaf ""));
     ("any-char", fun dict -> pack nt_any (fun a -> (Leaf (String.make 1 a))))];;
    
    
  let gen_grammar (prods : production list) =
    let dict = List.fold_left add_prod [] prods in
    let dict = (framework ())@dict in
    fun nt -> ((List.assoc nt dict) dict);;

  let compile_bnf bnf entry =
    gen_grammar (parse _grammar_ bnf) entry;;

  let compile_bnf_file file entry =
    compile_bnf (file_to_string file) entry;;
end;;
