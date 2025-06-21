type ast =
  | Main of ast
  | Statement_list of ast list
  | EmptyStatement
  | LineStatement of ast
  | BlockStatement of ast
  | ConditionalStatement of ast
  | LoopStatement of ast
  | Comment
  | Block of ast
  | Line of ast
  | Add of ast * ast
  | Mul of ast * ast
  | Sub of ast * ast
  | Div of ast * ast
  | Exponent of ast * ast
  | VectorLookup of string * ast
  | MatrixLookup of string * ast * ast
  | Rem of ast * ast
  | Neg of ast
  | Parenthesize of ast
  | Dimension of ast
  | Magnitude of ast
  | Angle of ast * ast
  | Determinant of ast
  | BoolOr of ast * ast
  | BoolAnd of ast * ast
  | BoolNot of ast
  | Equality of ast * ast
  | NEquality of ast * ast
  | Greater of ast * ast
  | Less of ast * ast
  | GEquality of ast * ast
  | LEquality of ast * ast
  | Absolute of ast
  | Transpose of ast
  | Lookup of string
  | Vector of ast
  | Matrix of ast
  | Explist of ast list
  | Vlist of ast list
  | IntConst of int
  | FloatConst of float
  | BoolConst of bool
  | SingleDeclaration of singletype * string
  | VectorDeclaration of singletype * string * ast
  | MatrixDeclaration of singletype * string * ast * ast
  | DeclAssignment of ast * ast
  | DeclInpAssignment of ast * ast
  | Assignment of string * ast
  | InpAssignment of string * ast
  | VectorElemAssignment of string * ast * ast
  | VectorElemInpAssignment of string * ast * ast
  | MatrixElemAssignment of string * ast * ast * ast
  | MatrixElemInpAssignment of string * ast * ast * ast
  | Print of string
  | TerminalInput
  | FileInput of string
  | IfElse of ast * ast * ast | For of ast * ast * ast * ast
  | While of ast * ast

and singletype =
  | TypeInt
  | TypeBool
  | TypeFloat
  

let rec pretty_print indent ast =
    let indent_str = String.make (indent * 4) ' ' in
    let next_indent = indent + 1 in
    let next_indent_str = String.make (next_indent * 4) ' ' in
    match ast with
    | Main ast -> 
        "Main (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | EmptyStatement ->
        "; "
    | Statement_list stmts ->
        "Statement_list [\n" ^ 
        String.concat ";\n" (List.map (fun stmt -> next_indent_str ^ pretty_print next_indent stmt) stmts) ^
        "\n" ^ indent_str ^ "]"
    | LineStatement ast -> 
        "LineStatement (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | BlockStatement ast -> 
        "BlockStatement (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | ConditionalStatement ast -> 
        "ConditionalStatement (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | LoopStatement ast -> 
        "LoopStatement (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Comment -> "Comment"
    | Block ast -> 
        "Block (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Line ast -> 
        "Line (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Add (lhs, rhs) -> 
        "Add (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
  
    | VectorLookup(a,b) ->
        "VectorLookup (\n" ^
        next_indent_str ^ a ^  ",\n" ^
        next_indent_str ^ pretty_print next_indent b ^ "\n" ^
        indent_str ^ ")"
    | MatrixLookup(a,b,c) ->
        "MatrixLookup (\n" ^
        next_indent_str ^ a ^ ",\n" ^
        next_indent_str ^ pretty_print next_indent b ^ ",\n" ^
        next_indent_str ^ pretty_print next_indent c ^ "\n" ^
        indent_str ^ ")"
    | Mul (lhs, rhs) -> 
        "Mul (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Sub (lhs, rhs) -> 
        "Sub (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Div (lhs, rhs) -> 
        "Div (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Exponent (lhs, rhs) -> 
        "Exponent (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Rem (lhs, rhs) -> 
        "Rem (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Neg ast -> 
        "Neg (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Parenthesize ast -> 
        "Parenthesize (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Dimension ast -> 
        "Dimension (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Magnitude ast -> 
        "Magnitude (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Angle (lhs, rhs) -> 
        "Angle (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Determinant ast -> 
        "Determinant (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | BoolOr (lhs, rhs) -> 
        "BoolOr (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | BoolAnd (lhs, rhs) -> 
        "BoolAnd (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | BoolNot ast -> 
        "BoolNot (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Equality (lhs, rhs) -> 
        "Equality (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | NEquality (lhs, rhs) -> 
        "NEquality (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Greater (lhs, rhs) -> 
        "Greater (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Less (lhs, rhs) -> 
        "Less (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | GEquality (lhs, rhs) -> 
        "GEquality (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | LEquality (lhs, rhs) -> 
        "LEquality (\n" ^ 
        next_indent_str ^ pretty_print next_indent lhs ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent rhs ^ "\n" ^ 
        indent_str ^ ")"
    | Absolute ast -> 
        "Absolute (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Transpose ast -> 
        "Transpose (\n" ^ 
        next_indent_str ^ pretty_print next_indent ast ^ "\n" ^ 
        indent_str ^ ")"
    | Lookup name -> "Lookup \"" ^ name ^ "\""
    | Vector expr -> 
        "Vector (\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | Matrix expr -> 
        "Matrix (\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | Explist exprs ->
        "Explist [\n" ^ 
        String.concat ";\n" (List.map (fun expr -> next_indent_str ^ pretty_print next_indent expr) exprs) ^
        "\n" ^ indent_str ^ "]"
    | Vlist exprs ->
        "Vlist [\n" ^ 
        String.concat ";\n" (List.map (fun expr -> next_indent_str ^ pretty_print next_indent expr) exprs) ^
        "\n" ^ indent_str ^ "]"
    | IntConst n -> "IntConst " ^ string_of_int n
    | FloatConst f -> "FloatConst " ^ string_of_float f
    | BoolConst b -> "BoolConst " ^ string_of_bool b
    | SingleDeclaration (typ, name) -> "SingleDeclaration (" ^ string_of_singletype typ ^ ", \"" ^ name ^ "\")"
    | VectorDeclaration (typ, name, size) -> 
        "VectorDeclaration (" ^ string_of_singletype typ ^ ", \"" ^ name ^ "\",\n" ^ 
        next_indent_str ^ pretty_print next_indent size ^ "\n" ^ 
        indent_str ^ ")"
    | MatrixDeclaration (typ, name, rows, cols) -> 
        "MatrixDeclaration (" ^ string_of_singletype typ ^ ", \"" ^ name ^ "\",\n" ^ 
        next_indent_str ^ pretty_print next_indent rows ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent cols ^ "\n" ^ 
        indent_str ^ ")"
    | DeclAssignment (decl, expr) -> 
        "DeclAssignment (\n" ^ 
        next_indent_str ^ pretty_print next_indent decl ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | DeclInpAssignment (decl, expr) -> 
        "DeclInpAssignment (\n" ^ 
        next_indent_str ^ pretty_print next_indent decl ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | Assignment (name, expr) -> 
        "Assignment (\"" ^ name ^ "\",\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | InpAssignment (name, expr) -> 
        "InpAssignment (\"" ^ name ^ "\",\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | VectorElemAssignment (name, idx, expr) -> 
        "VectorElemAssignment (\"" ^ name ^ "\",\n" ^ 
        next_indent_str ^ pretty_print next_indent idx ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | VectorElemInpAssignment (name, idx, expr) -> 
        "VectorElemInpAssignment (\"" ^ name ^ "\",\n" ^ 
        next_indent_str ^ pretty_print next_indent idx ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | MatrixElemAssignment (name, row, col, expr) -> 
        "MatrixElemAssignment (\"" ^ name ^ "\",\n" ^ 
        next_indent_str ^ pretty_print next_indent row ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent col ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | MatrixElemInpAssignment (name, row, col, expr) -> 
        "MatrixElemInpAssignment (\"" ^ name ^ "\",\n" ^ 
        next_indent_str ^ pretty_print next_indent row ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent col ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent expr ^ "\n" ^ 
        indent_str ^ ")"
    | Print msg -> "Print \"" ^ msg ^ "\""
    | TerminalInput -> "TerminalInput"
    | FileInput path -> "FileInput \"" ^ path ^ "\""
    | IfElse (cond, then_block, else_block) -> 
        "IfElse (\n" ^ 
        next_indent_str ^ pretty_print next_indent cond ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent then_block ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent else_block ^ "\n" ^ 
        indent_str ^ ")"
    | For (init, cond, update, body) -> 
        "For (\n" ^ 
        next_indent_str ^ pretty_print next_indent init ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent cond ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent update ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent body ^ "\n" ^ 
        indent_str ^ ")"
    | While (cond, body) -> 
        "While (\n" ^ 
        next_indent_str ^ pretty_print next_indent cond ^ ",\n" ^ 
        next_indent_str ^ pretty_print next_indent body ^ "\n" ^ 
        indent_str ^ ")"
  
  and string_of_singletype = function
    | TypeInt -> "TypeInt"
    | TypeBool -> "TypeBool"
    | TypeFloat -> "TypeFloat"
  
  
  (* This function returns a string representation of the AST *)
let print_ast ast =
    pretty_print 0 ast
  
  
  
  
  
  