open Printf;;

module BNFParser = struct 
  type mods =
    | ModStar
    | ModPlus
    | ModMaybe
    | ModSkip
    | ModTrace
        
  type callback =
    | Default
    | Callback of string
                    
  type expr =
    | Range of char * char
    | Lit of string
    | LitCi of string
    | Nt of string
    | Star of expr
    | Plus of expr
    | Maybe of expr
    | Skip of expr
    | Trace of expr
    | Diff of expr * expr
    | Caten of expr list
    | Disj of expr list
    | Expr of callback * expr

  type production = Prod of string * expr;;

  exception Bad_nonterm_exception of expr;;
  exception Internal_parser_error of string;;

  let rec expr_to_string = function
    | Range(c1,c2) -> sprintf "'%c'-'%c'" c1 c2
    | Lit(s) -> sprintf "'%s'" s
    | LitCi(s) -> sprintf "`%s`" s
    | Nt(nt) -> sprintf "<%s>" nt
    | Star(e) -> sprintf "(%s)*" (expr_to_string e)
    | Plus(e) -> sprintf "(%s)+" (expr_to_string e)
    | Maybe(e) -> sprintf "(%s)?" (expr_to_string e)
    | Skip(e) -> sprintf "(%s)#" (expr_to_string e)
    | Trace(e) -> sprintf "(%s)$" (expr_to_string e)
    | Diff(e1,e2) -> sprintf "%s \ %s" (expr_to_string e1) (expr_to_string e2)
    | Caten(es) -> sprintf "%s" (String.join " " (List.map expr_to_string es))
    | Disj(es) -> sprintf "%s" (String.join " | " (List.map expr_to_string es))
    | Expr(Callback(cb), e) -> sprintf "%s {%s}" (expr_to_string e) cb
    | Expr(Default, e) -> sprintf "%s" (expr_to_string e)
                                  

  let _newline_ = char '\n';;

  let _comment_ =
    let _prefix_ = char ';' in
    let _any_ = nt_any in
    let _no_newline_ = diff _any_ _newline_ in
    let _stared_ = star _no_newline_ in
    pack (caten _prefix_ _stared_) (fun _ -> ());;

  let _whitespace_ =
    let _space_ = pack (one_of " \012\r\t") (fun _ -> ()) in
    disj _comment_ _space_;;

  let make_spaced =
    fun _p_ -> make_padded _whitespace_ _p_;;
    
  let _alpha_ = range_ci 'a' 'z';;

  let _alphanum_ =
    let _num_ = range '0' '9' in
    disj _num_ _alpha_ ;;
  let _expand_to_ = make_spaced (one_of_words ["::="; ":="; "="; "->"; ":"]);;

  let _cb_lparen_ = make_spaced (char '{');;
  let _cb_rparen_ = make_spaced (char '}');;
  let _disj_op_ = make_spaced (char '|');;
  let _caten_op_ = star _whitespace_;;
  let _diff_op_ = make_spaced (char '\\');;
    
  let _kstar_ = pack (caten (char '*') (star _whitespace_)) (fun _ -> ModStar);;
  let _kplus_ = pack (caten (char '+') (star _whitespace_)) (fun _ -> ModPlus);;
  let _maybe_ = pack (caten (char '?') (star _whitespace_)) (fun _ -> ModMaybe);;
  let _skip_ = pack (caten (char '#') (star _whitespace_)) (fun _ -> ModSkip);;
  let _trace_ = pack (caten (char '$') (star _whitespace_)) (fun _ -> ModTrace);;

  let _nest_lbracket_ = make_spaced (char '[');;
  let _nest_rbracket_ = make_spaced (char ']');;
  let _nest_lparen_ = make_spaced (char '(');;
  let _nest_rparen_ = make_spaced (char ')');;

  let _nt_langle_ = caten (star _whitespace_) (char '<');;
  let _nt_rangle_ = caten (star _whitespace_) (char '>');;
  let _lit_lquote_ = caten (star _whitespace_) (char '\'');;
  let _lit_rquote_ = caten (star _whitespace_) (char '\'');;

  let _lit_ci_lquote_ = caten (star _whitespace_) (char '`');;
  let _lit_ci_rquote_ = caten (star _whitespace_) (char '`');;
    
  let _escaped_ = one_of "`'\\";;
  let _range_hyphen_ = char '-';;

  let _prod_term_ = char '\n';;


  let _ident_ =
    let _prefix_ = _alpha_ in
    let _postfix_ = star (disj _alphanum_ (one_of "_- ")) in
    pack (caten _prefix_ _postfix_) (fun (pre, post) -> String.of_list (pre::post));;
    
  let _escape_ =
    pack (caten (char '\\') _escaped_) cdr;;

  let _lit_char_ =
    let _any_ = nt_any in
    let _simple_char_ = diff _any_ _escaped_ in
    disj _simple_char_ _escape_;;

  let _range_char_ =
    make_enclosed _lit_lquote_ _lit_char_ _lit_rquote_;;

  let _range_ =
    let _range_ = caten _range_char_ (caten _range_hyphen_ _range_char_) in
    pack _range_ (fun (x, (_, y)) -> Range(x,y));;

  let _lit_ci_ =
    let _chars_ = star _lit_char_ in
    let _lit_ = make_enclosed _lit_ci_lquote_ _chars_ _lit_ci_rquote_ in
    pack _lit_ (fun chars -> LitCi (String.of_list chars));;
    
  let _lit_ =
    let _chars_ = star _lit_char_ in
    let _lit_ = make_enclosed _lit_lquote_ _chars_ _lit_rquote_ in
    pack _lit_ (fun chars -> Lit (String.of_list chars));;

  let _literal_ = disj _lit_ _lit_ci_;;
    
  let _term_ =
    disj _range_ _literal_;;

  let _nt_ =
    let _nt_ = caten _nt_langle_ (caten _ident_ _nt_rangle_) in
    let _nt_ = pack _nt_ cadr in
    pack _nt_ (fun nt -> Nt nt);;

  let _modifier_ =
    disj_list [_kstar_; _kplus_; _maybe_ ; _skip_; _trace_];;

  let construct_mod modifier expr =
    match modifier with
    | ModStar -> Star expr
    | ModPlus -> Plus expr
    | ModMaybe -> Maybe expr
    | ModSkip -> Skip expr
    | ModTrace -> Trace expr
                        
  let _callback_ =
    make_enclosed _cb_lparen_ _ident_ _cb_rparen_;;

    
  let rec _nested_expr_ s =
    let nest _l_ _r_ = make_enclosed _l_ _expr_ _r_ in
    let _brackets_ = nest _nest_lbracket_ _nest_rbracket_ in
    let _parens_ = nest _nest_lparen_ _nest_rparen_ in
    (disj _brackets_ _parens_) s
                               
                               
  and _operand_ s =
    let _operand_ = disj_list [_nt_; _term_; _nested_expr_] in
    let _mod_ = maybe _modifier_ in
    let _modded_ = caten _operand_ _mod_ in
    let _packed_ = pack _modded_ (fun (o, modifier) ->
                          match modifier with 
                          | Some(m) -> construct_mod m o
                          | None -> o) in
    _packed_ s
             
  and _diff_ s =
    let _op_ = _operand_ in
    let _subtrahend_ =  pack (caten _diff_op_ _op_) cdr in
    let _diff_ = caten _op_ (maybe _subtrahend_) in
    let _packed_ = pack _diff_ (fun (op, sub) ->
                          match sub with
                          | Some(s) -> Diff(op, s)
                          | None -> op) in
    _packed_ s

  and _caten_ s =
    let _op_ = _diff_ in
    let _ops_ = star (pack (caten _caten_op_ _op_) cdr) in
    let _caten_ = caten _op_ _ops_ in
    let _packed_ = pack _caten_ (fun (o, os) ->
                          if os = [] then o else Caten(o::os)) in
    _packed_ s

  and _disj_ s =
    let _op_ = _caten_ in
    let _ops_ = star (pack (caten _disj_op_ _op_) cdr) in
    let _disj_ = caten _op_ _ops_ in
    let _packed_ = pack _disj_ (fun (o, os) ->
                          if os = [] then o else Disj(o::os)) in
    _packed_ s

             
  and _expr_ s =
    let _cb_ = maybe _callback_ in
    let _modded_ = pack (caten _disj_ (maybe _trace_)) (fun (disj, m) -> match m with
                                                                         | None -> disj
                                                                         | Some(ModTrace) -> Trace disj
                                                                         | _ -> raise (Internal_parser_error "Unexpected modified in <expr> parser")) in
    let _expr_ = caten _modded_ _cb_ in
    let _packed_ = pack _expr_ (fun (disj, cb) -> match cb with
                                                  | Some(cb) -> Expr(Callback(cb), disj)
                                                  | None -> Expr(Default, disj)) in
    _packed_ s;;

  let _lhs_ = _nt_;;
  let _rhs_ = _expr_;;

  let _production_ =
    let _lhs_ = caten _lhs_ (maybe _trace_) in
    let _prod_ = caten _lhs_ (caten _expand_to_ (caten _rhs_ (star _prod_term_))) in
    pack _prod_ (fun ((l, t), (_, (r, _))) ->
           match l, t with
           | Nt(n), None -> Prod(n, r)
           | Nt(n), Some(ModTrace) -> Prod(n, Trace r)
           | _,_ -> raise (Bad_nonterm_exception(l)));;

  let _grammar_ =
    let _empty_lines_ = star (make_spaced _newline_) in
    let _prod_ = pack (caten _empty_lines_ _production_) cdr in
    star _prod_;;

    
    
end;;
