open Ast

let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.main Lexer.tokenize lexbuf in

    print_endline (print_ast ast);
    let _ = Type_checker.main ast in
    Printf.printf "Parsing successful\n"
    
    


  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "Lexing error: %s\n" msg;
      exit 1
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.fprintf stderr "Syntax error at line %d, position %d\n" 
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
      exit 1
  | Type_checker.TypeError (ast,msg) ->
      Printf.fprintf stderr "Type error: %s\n" msg;
      Printf.fprintf stderr "AST: %s\n" (print_ast ast);
      exit 1

  | Type_checker.DimensionError (ast,msg)->
      Printf.fprintf stderr "Dimension error: %s\n" msg;
      Printf.fprintf stderr "AST: %s\n" (print_ast ast);
      exit 1

  | Type_checker.VariableNotFound (var,msg) ->
    Printf.fprintf stderr "Variable not found error: %s\n" msg;
    exit 1

  | Type_checker.OverwriteError (var,msg) ->
    Printf.fprintf stderr "Overwrite error: %s\n" msg;
    exit 1

  | Type_checker.ScopeNotFound (ast,msg) ->
    Printf.fprintf stderr "Scope error: %s\n" msg;
    Printf.fprintf stderr "AST: %s\n" (print_ast ast);
    exit 1

  | e ->
      Printf.fprintf stderr "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1  
