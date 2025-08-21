open Ast

let space = " "
let suffix = ""

let rec emit_expr expr =
  let emit_args args =
    let args = List.rev args in
    match args with
    | [] -> " () "
    | _ -> List.fold_right (fun arg acc -> acc ^ space ^ arg ^ space) args ""
  in
  match expr with
  | EUnit -> " () "
  | EChar c -> String.make 1 c
  | EInt n -> string_of_int n
  | EFloat n -> string_of_float n
  | EVar name -> name ^ suffix
  | ECall (expr, args) ->
    (match expr with
     | EVar name ->
       name ^ suffix ^ space ^ emit_args (List.map (fun a -> emit_expr a) args)
     | _ -> failwith "Unsupported call expr")
  | EBlock (stmts, expr) ->
    let stmts = List.rev stmts in
    List.fold_right (fun stmt acc -> acc ^ emit_stmt stmt) stmts "" ^ emit_expr expr

and emit_stmt stmt =
  match stmt with
  | SLet (name, expr) -> "let rec " ^ name ^ suffix ^ " = " ^ emit_expr expr ^ " in "
  | SExpr expr -> emit_expr expr ^ "; "
  | SFun (name, args, block) ->
    let emit_args args =
      let args = List.rev args in
      match args with
      | [] -> " () "
      | _ -> List.fold_right (fun arg acc -> acc ^ arg ^ suffix ^ " ") args ""
    in
    "let rec " ^ name ^ suffix ^ space ^ emit_args args ^ " = " ^ emit_expr block ^ "; "

and emit_decl decl =
  let emit_params params =
    let params = List.rev params in
    match params with
    | [] -> " () "
    | _ -> List.fold_right (fun arg acc -> acc ^ arg ^ suffix ^ space) params ""
  in
  match decl with
  | DFun (name, args, block) ->
    (match name with
     | "main" -> "let () = " ^ emit_expr block
     | _ ->
       "let rec " ^ name ^ suffix ^ space ^ emit_params args ^ " = " ^ emit_expr block)
;;

let emit_program name program =
  let program =
    let program = List.rev program in
    List.fold_right (fun decl acc -> acc ^ emit_decl decl ^ space) program ""
  in
  let write_file filename content =
    let oc = open_out filename in
    try
      output_string oc content;
      flush oc;
      close_out oc
    with
    | e ->
      close_out_noerr oc;
      raise e
  in
  write_file name program
;;
