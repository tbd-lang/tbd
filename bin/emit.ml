open Ast

let rec emit_expr expr =
  match expr with
  | EUnit -> " () "
  | EChar c -> " " ^ String.make 1 c ^ " "
  | EInt n -> " " ^ string_of_int n ^ " "
  | EFloat n -> " " ^ string_of_float n ^ " "
  | EBlock (decls, expr) ->
    let decls = List.rev decls in
    List.fold_right (fun decl acc -> acc ^ emit_decl decl) decls "" ^ emit_expr expr

and emit_decl decl =
  let emit_args args =
    let args = List.rev args in
    match args with
    | [] -> " () "
    | _ -> List.fold_right (fun arg acc -> acc ^ " " ^ arg ^ " ") args ""
  in
  match decl with
  | DFun (name, args, block) ->
    (match name with
     | "main" -> "let () = " ^ emit_expr block
     | _ -> "let rec " ^ "tbd__" ^ name ^ emit_args args ^ "=" ^ emit_expr block)
  | DLet (name, expr) -> "let rec " ^ name ^ "=" ^ emit_expr expr ^ " in "
;;

let emit_program name program =
  let program =
    let program = List.rev program in
    List.fold_right (fun decl acc -> acc ^ " " ^ emit_decl decl ^ " ") program ""
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
