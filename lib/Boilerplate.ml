(**
   Boilerplate to be used as a template when mapping the lua CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_string_start (env : env) (tok : CST.string_start) =
  (* string_start *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_string_end (env : env) (tok : CST.string_end) =
  (* string_end *) token env tok

let map_shebang (env : env) (tok : CST.shebang) =
  (* pattern #!.* *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env tok

let map_number (env : env) (tok : CST.number) =
  (* number *) token env tok

let map_comment_end (env : env) (tok : CST.comment_end) =
  (* comment_end *) token env tok

let map_comment_content (env : env) (tok : CST.comment_content) =
  (* comment_content *) token env tok

let map_field_separator (env : env) (x : CST.field_separator) =
  (match x with
  | `COMMA tok -> R.Case ("COMMA",
      (* "," *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_comment_start (env : env) (tok : CST.comment_start) =
  (* comment_start *) token env tok

let map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = (* string_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* string_content *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_function_identifier (env : env) ((v1, v2, v3) : CST.function_identifier) =
  let v1 =
    (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 =
          (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_name_list (env : env) ((v1, v2) : CST.name_list) =
  let v1 =
    (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_parameter_list (env : env) (x : CST.parameter_list) =
  (match x with
  | `Id_rep_COMMA_id_opt_COMMA_vararg_exp (v1, v2, v3) -> R.Case ("Id_rep_COMMA_id_opt_COMMA_vararg_exp",
      let v1 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
      in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
          in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = (* "..." *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Vararg_exp tok -> R.Case ("Vararg_exp",
      (* "..." *) token env tok
    )
  )

let map_local_variable (env : env) ((v1, v2) : CST.local_variable) =
  let v1 =
    (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_local_variable_list (env : env) ((v1, v2) : CST.local_variable_list) =
  let v1 = map_local_variable env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_local_variable env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let rec map_argument_list (env : env) (x : CST.argument_list) =
  (match x with
  | `LPAR_opt_exp_list_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_exp_list_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression_list env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Table x -> R.Case ("Table",
      map_table env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_or_exp (v1, v2, v3) -> R.Case ("Exp_or_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_and_exp (v1, v2, v3) -> R.Case ("Exp_and_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_TILDEEQ_exp (v1, v2, v3) -> R.Case ("Exp_TILDEEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "~=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_TILDE_exp (v1, v2, v3) -> R.Case ("Exp_TILDE_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASHSLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASHSLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "//" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DOTDOT_exp (v1, v2, v3) -> R.Case ("Exp_DOTDOT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ".." *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) (x : CST.block) =
  (match x with
  | `Ret_stmt x -> R.Case ("Ret_stmt",
      map_return_statement env x
    )
  | `Rep1_stmt_opt_ret_stmt (v1, v2) -> R.Case ("Rep1_stmt_opt_ret_stmt",
      let v1 = R.List (List.map (map_statement env) v1) in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_return_statement env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_block_ (env : env) (x : CST.block_) =
  map_block env x

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_elseif_clause (env : env) ((v1, v2, v3, v4) : CST.elseif_clause) =
  let v1 = (* "elseif" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "then" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_block_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Nil tok -> R.Case ("Nil",
      (* "nil" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `Num tok -> R.Case ("Num",
      (* number *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Vararg_exp tok -> R.Case ("Vararg_exp",
      (* "..." *) token env tok
    )
  | `Func_defi (v1, v2) -> R.Case ("Func_defi",
      let v1 = (* "function" *) token env v1 in
      let v2 = map_function_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_var x -> R.Case ("Choice_var",
      map_prefix_expression env x
    )
  | `Table x -> R.Case ("Table",
      map_table env x
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  )

and map_expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_field (env : env) (x : CST.field) =
  (match x with
  | `LBRACK_exp_RBRACK_EQ_exp (v1, v2, v3, v4, v5) -> R.Case ("LBRACK_exp_RBRACK_EQ_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Id_EQ_exp (v1, v2, v3) -> R.Case ("Id_EQ_exp",
      let v1 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_field_list (env : env) ((v1, v2, v3) : CST.field_list) =
  let v1 = map_field env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_field_separator env v1 in
      let v2 = map_field env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_field_separator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_function_body (env : env) ((v1, v2, v3, v4, v5) : CST.function_body) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_block_ env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_function_call (env : env) ((v1, v2, v3) : CST.function_call) =
  let v1 = map_prefix_expression_ env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 =
          (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_argument_list env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_prefix_expression (env : env) (x : CST.prefix_expression) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `Func_call x -> R.Case ("Func_call",
      map_function_call env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_prefix_expression_ (env : env) (x : CST.prefix_expression_) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `Func_call x -> R.Case ("Func_call",
      map_function_call env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expression_list env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Empty_stmt tok -> R.Case ("Empty_stmt",
      (* ";" *) token env tok
    )
  | `Var_assign (v1, v2, v3) -> R.Case ("Var_assign",
      let v1 = map_variable_list env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_value_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Local_var_decl (v1, v2, v3) -> R.Case ("Local_var_decl",
      let v1 = (* "local" *) token env v1 in
      let v2 = map_local_variable_list env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_value_list env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Func_call x -> R.Case ("Func_call",
      map_function_call env x
    )
  | `Label_stmt (v1, v2, v3) -> R.Case ("Label_stmt",
      let v1 = (* "::" *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      let v3 = (* "::" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Goto_stmt (v1, v2) -> R.Case ("Goto_stmt",
      let v1 = (* "goto" *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `Brk_stmt tok -> R.Case ("Brk_stmt",
      (* "break" *) token env tok
    )
  | `Do_stmt (v1, v2, v3) -> R.Case ("Do_stmt",
      let v1 = (* "do" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_block_ env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "end" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `While_stmt (v1, v2, v3, v4, v5) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "do" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_block_ env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Repeat_stmt (v1, v2, v3, v4) -> R.Case ("Repeat_stmt",
      let v1 = (* "repeat" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_block_ env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "until" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "then" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_block_ env x
          ))
        | None -> R.Option None)
      in
      let v5 = R.List (List.map (map_elseif_clause env) v5) in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_else_clause env x
          ))
        | None -> R.Option None)
      in
      let v7 = (* "end" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `For_nume_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> R.Case ("For_nume_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v2
      in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_expression env v4 in
      let v5 = (* "," *) token env v5 in
      let v6 = map_expression env v6 in
      let v7 =
        (match v7 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v8 = (* "do" *) token env v8 in
      let v9 =
        (match v9 with
        | Some x -> R.Option (Some (
            map_block_ env x
          ))
        | None -> R.Option None)
      in
      let v10 = (* "end" *) token env v10 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]
    )
  | `For_gene_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("For_gene_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = map_name_list env v2 in
      let v3 = (* "in" *) token env v3 in
      let v4 = map_value_list env v4 in
      let v5 = (* "do" *) token env v5 in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_block_ env x
          ))
        | None -> R.Option None)
      in
      let v7 = (* "end" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Func_defi_stmt (v1, v2, v3) -> R.Case ("Func_defi_stmt",
      let v1 = (* "function" *) token env v1 in
      let v2 = map_function_identifier env v2 in
      let v3 = map_function_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Local_func_defi_stmt (v1, v2, v3, v4) -> R.Case ("Local_func_defi_stmt",
      let v1 = (* "local" *) token env v1 in
      let v2 = (* "function" *) token env v2 in
      let v3 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v3
      in
      let v4 = map_function_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_table (env : env) ((v1, v2, v3) : CST.table) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_field_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Not_exp (v1, v2) -> R.Case ("Not_exp",
      let v1 = (* "not" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `HASH_exp (v1, v2) -> R.Case ("HASH_exp",
      let v1 = (* "#" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASH_exp (v1, v2) -> R.Case ("DASH_exp",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `TILDE_exp (v1, v2) -> R.Case ("TILDE_exp",
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_value_list (env : env) ((v1, v2) : CST.value_list) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env tok
    )
  | `Prefix_exp__LBRACK_exp_RBRACK (v1, v2, v3, v4) -> R.Case ("Prefix_exp__LBRACK_exp_RBRACK",
      let v1 = map_prefix_expression_ env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Prefix_exp__DOT_id (v1, v2, v3) -> R.Case ("Prefix_exp__DOT_id",
      let v1 = map_prefix_expression_ env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_variable_list (env : env) ((v1, v2) : CST.variable_list) =
  let v1 = map_variable env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_chunk (env : env) ((v1, v2) : CST.chunk) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* pattern #!.* *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_comment (env : env) ((v1, v2, v3) : CST.comment) =
  let v1 = (* comment_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* comment_content *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* comment_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let dump_tree root =
  map_chunk () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
