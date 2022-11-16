#use "utils.ml";;
#use "parser_utils.ml";;

module UglyParser = struct 


  type var = string;;
    
  type expr =
    | Num of int
    | List of expr list
    | Lit of string
    | LitCI of string
    | Nt of string
    | Var of var;;
    
  type definition =
    | ProdBy of var * expr
    | RHSOf of var * expr;;

  type condition =
    | LT of bool  * var * expr
    | GT of bool * var * expr
    | EQ of bool * var * expr
    | LTE of bool * var * expr
    | GTE of bool * var * expr
    | NEQ of bool * var * expr
    | Contains of bool * var * expr
    | EndsWith of bool * var * expr
    | StartsWith of bool * var * expr;;
        
  type statement =
    | Setting of string * expr 
    | Op of string * expr list
    | Let of definition
    | Under of bool * expr * statement list
    | If of condition * statement list;;

  exception Unsupported_statement of statement;;
  exception Unexpected_expr of expr * string

  let _comment_ =
    let _prefix_ = char ';' in
    let _any_ = nt_any in
    let _no_newline_ = diff _any_ (char '\n') in
    pack (caten _prefix_ (star _no_newline_)) (fun _ -> ());;

  let _space_ =
    let _whitespace_ = pack (one_of " \012\n\t") (fun _ -> ()) in
    disj _whitespace_ _comment_

  let make_spaced _p_ = make_padded _space_ _p_;;
  let make_spaced_plus _p_ = enclose_with _p_
                                          (star _space_)
                                          (plus _space_);;
    
  let _LET_ = make_spaced_plus (word "let");;
  let _NOT_ = make_spaced (disj (word "not") (word "!"));;
  let _UNDER_ = make_spaced_plus (word "under");;
  let _IF_ = make_spaced_plus (word "if");;

  let _LT_ = make_spaced (char '<');;
  let _GT_ = make_spaced (char '>');;
  let _LTE_ = make_spaced (word "<=");;
  let _GTE_ = make_spaced (word ">=");;
  let _EQ_ = make_spaced (char '=');;
  let _NEQ_ = make_spaced (disj (word "<>") (word "!="));;
  let _CONTAINS_ = make_spaced (word "contains");;
  let _STARTSWITH_ = make_spaced (word "startswith");;
  let _ENDSWITH_ = make_spaced (word "endswith");;

  let _OP_L_BRACKET_ = make_spaced (char '(');;
  let _OP_R_BRACKET_ = make_spaced (char ')');;
  let _NT_L_BRACKET_ = make_spaced (char '<');;
  let _NT_R_BRACKET_ = make_spaced (char '>');;
  let _LIST_L_BRACKET_ = make_spaced (char '[');;
  let _LIST_R_BRACKET_ = make_spaced (char ']');;
  let _BLOCK_L_BRACKET_ = make_spaced (char '{');;
  let _BLOCK_R_BRACKET_ = make_spaced (char '}');;
  let _LIT_L_BRACKET_ = caten (star _space_) (char '\'');;
  let _LIT_R_BRACKET_ = caten (char '\'') (star _space_);;
  let _LIT_CI_L_BRACKET_ = caten (star _space_) (char '`');;
  let _LIT_CI_R_BRACKET_ = caten (char '`') (star _space_);;
  let _LIST_SEP_ = make_spaced (char ',');;
  let _ARG_SEP_ = make_spaced (char ',');;
    
  let _GLOBAL_SETTINGS_PREFIX_ =
    let _space_ = star _space_ in
    pack (caten _space_ (char '#')) (fun _ -> '#');;
  let _GLOBAL_ASSIGN_ = make_spaced (char '=');;

  let _PRODUCED_BY_ = make_spaced (char ':');;
  let _RHS_OF_ = make_spaced (word "::");;

  let _DIGIT_ = range '0' '9';;
  let _ALPHA_ = range_ci 'a' 'z';;
  let _ALPHANUM_ = disj _ALPHA_ _DIGIT_;;

  let _num_ =
    let _num_ = pack (plus _DIGIT_) (fun ds -> int_of_string (list_to_string ds)) in
    pack (make_spaced _num_) (fun n -> Num n);;
    
  let _ident_ = 
    let _ident_ = disj _ALPHANUM_  (char '_') in
    let _ident_ = pack (plus _ident_) list_to_string in
    make_spaced _ident_ 
                     
  let _nt_ =
    let _nt_name_ = plus (disj_list [_ALPHANUM_; one_of " _-"]) in
    let _nt_ = enclose_with _nt_name_ _NT_L_BRACKET_ _NT_R_BRACKET_ in
    pack _nt_ (fun s -> Nt (list_to_string s));;

  let _lit_ =
    let _any_ = diff nt_any _LIT_R_BRACKET_ in
    let _lit_ = enclose_with (star _any_) _LIT_L_BRACKET_  _LIT_R_BRACKET_ in
    pack _lit_ (fun s -> Lit (list_to_string s));;

  let _lit_ci_ =
    let _any_ = diff nt_any _LIT_CI_R_BRACKET_ in
    let _lit_ci_ = enclose_with (star _any_) _LIT_CI_L_BRACKET_ _LIT_CI_R_BRACKET_ in
    pack _lit_ci_ (fun s -> LitCI (list_to_string s));;

  let _literal_ = disj _lit_ _lit_ci_;;

  let _var_ = _ident_;;

  let _simple_value_ = disj _nt_ _literal_;;
    
  let _list_ =
    let _var_ = pack _var_ (fun s -> Var s) in
    let _element_ = disj _simple_value_ _var_ in
    let _elements_ = star (pack (caten _LIST_SEP_ _element_) cdr) in
    let _elements_ = pack (caten _element_ _elements_) (fun (e, es) ->  e::es) in
    let _elements_ = disj _elements_ nt_epsilon in
    pack (enclose_with _elements_ _LIST_L_BRACKET_ _LIST_R_BRACKET_)
         (fun es -> List es);;

  let _value_ = disj_list [_simple_value_; _list_; _num_];;

  let _expr_ = disj _value_ (pack _var_ (fun s -> Var s));;

  let _glob_setting_ =
    let _glob_var_ = enclose_with _ident_ _GLOBAL_SETTINGS_PREFIX_  _GLOBAL_ASSIGN_ in
    pack (caten _glob_var_ _value_) (fun (var, v) -> Setting(var, v));;
    
  let _arguments_ =
    let _argument_ = _expr_ in
    let _arguments_ = star (pack (caten _ARG_SEP_ _argument_) cdr) in
    let _arguments_ = pack (caten _argument_ _arguments_) (fun (arg, args) -> arg::args) in
    disj _arguments_ nt_epsilon;;

  let _op_ =
    let _args_ = enclose_with _arguments_ _OP_L_BRACKET_ _OP_R_BRACKET_ in
    pack (caten _ident_ _args_) (fun (ident,args) -> Op(ident, args));;
    
  let rec _let_ s = 
    let _prod_by_ = pack (caten _PRODUCED_BY_ _expr_) cdr in
    let _prod_by_ = pack (caten _var_ _prod_by_) (fun (v,e) -> ProdBy(v,e)) in
    let _rhs_of_ = pack (caten _RHS_OF_ _expr_) cdr in
    let _rhs_of_ = pack (caten _var_ _rhs_of_) (fun (v,e) -> RHSOf(v,e)) in
    let _var_def_ = disj _prod_by_ _rhs_of_ in
    let _var_ = pack (caten _LET_ _var_def_) cdr in
    let _p_ = pack _var_ (fun v -> Let v) in
    _p_ s;;

  let make_condition _op_ packer =
    let _maybe_not_ = pack (maybe _NOT_) (fun s -> match s with
                                                   | None -> true
                                                   | Some(_) -> false) in
    let _cond_ = caten _maybe_not_ _var_ in
    let _cond_ = pack (caten _cond_ _op_) car in
    pack (caten _cond_ _expr_) packer;;
    
  let _condition_ =
    disj_list [make_condition _LT_ (fun ((n, v), e) -> LT(n, v, e));
               make_condition _GT_ (fun ((n, v), e) -> GT(n, v, e));
               make_condition _EQ_ (fun ((n, v), e) -> EQ(n, v, e));
               make_condition _LTE_ (fun ((n, v), e) -> LTE(n, v, e));
               make_condition _GTE_ (fun ((n, v), e) -> GTE(n, v, e));
               make_condition _NEQ_ (fun ((n, v), e) -> NEQ(n, v, e));
               make_condition _CONTAINS_ (fun ((n, v), e) -> Contains(n, v, e));
               make_condition _ENDSWITH_ (fun ((n, v), e) -> EndsWith(n, v, e));
               make_condition _STARTSWITH_ (fun ((n, v), e) -> StartsWith(n, v, e))];;

    
  let rec _under_ (s : char list) : statement * char list =               
    let _not_ = pack (maybe _NOT_) (fun n -> match n with 
                                              | Some _ -> false
                                              | None -> true ) in
    let _not_ = pack (caten _not_ _UNDER_) car in
    let _under_ = caten _not_ _expr_ in
    let _p_ = pack (caten _under_ _block_) (fun ((n, e), b) -> Under(n, e, b)) in
    _p_ s
 
  and _if_ s =
    let _if_ = pack (caten _IF_ _condition_) cdr in
    let _p_ = pack (caten _if_ _block_) (fun (c, b) -> If (c, b)) in
    _p_ s
        
  and _block_ s =
    let _statements_ = star _statement_ in
    let _p_ = enclose_with _statements_ _BLOCK_L_BRACKET_ _BLOCK_R_BRACKET_ in
    _p_ s

  and _statement_ s =
    let _p_ = disj_list [_op_; _let_; _under_; _if_] in
    _p_ s;;

  let _ugly_ =
    star (disj _glob_setting_ _statement_);;
    
end;;
