open Ast

(* Value types that the interpreter can handle *)
type value = 
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
  | VectorVal of value array
  | MatrixVal of value array array
  | UnitVal

(* Environment for variable bindings *)
type environment = (string * value) list

(* Exception types for runtime errors *)
exception RuntimeError of string
exception TypeMismatch of string
exception DivisionByZero
exception IndexOutOfBounds
exception UndefinedVariable of string

(* Helper functions for type conversion and arithmetic *)
let to_float = function
  | IntVal i -> float_of_int i
  | FloatVal f -> f
  | _ -> raise (TypeMismatch "Expected numeric value")

let to_int = function
  | IntVal i -> i
  | FloatVal f -> int_of_float f
  | _ -> raise (TypeMismatch "Expected numeric value")

let to_bool = function
  | BoolVal b -> b
  | _ -> raise (TypeMismatch "Expected boolean value")

(* Arithmetic operations with type promotion *)
let add_values v1 v2 = 
  match v1, v2 with
  | IntVal i1, IntVal i2 -> IntVal (i1 + i2)
  | FloatVal f1, FloatVal f2 -> FloatVal (f1 +. f2)
  | IntVal i, FloatVal f -> FloatVal (float_of_int i +. f)
  | FloatVal f, IntVal i -> FloatVal (f +. float_of_int i)
  | _ -> raise (TypeMismatch "Cannot add non-numeric values")

let sub_values v1 v2 = 
  match v1, v2 with
  | IntVal i1, IntVal i2 -> IntVal (i1 - i2)
  | FloatVal f1, FloatVal f2 -> FloatVal (f1 -. f2)
  | IntVal i, FloatVal f -> FloatVal (float_of_int i -. f)
  | FloatVal f, IntVal i -> FloatVal (f -. float_of_int i)
  | _ -> raise (TypeMismatch "Cannot subtract non-numeric values")

let mul_values v1 v2 = 
  match v1, v2 with
  | IntVal i1, IntVal i2 -> IntVal (i1 * i2)
  | FloatVal f1, FloatVal f2 -> FloatVal (f1 *. f2)
  | IntVal i, FloatVal f -> FloatVal (float_of_int i *. f)
  | FloatVal f, IntVal i -> FloatVal (f *. float_of_int i)
  | _ -> raise (TypeMismatch "Cannot multiply non-numeric values")

let div_values v1 v2 = 
  match v1, v2 with
  | IntVal i1, IntVal i2 -> 
      if i2 = 0 then raise DivisionByZero
      else FloatVal (float_of_int i1 /. float_of_int i2)
  | FloatVal f1, FloatVal f2 -> 
      if f2 = 0.0 then raise DivisionByZero
      else FloatVal (f1 /. f2)
  | IntVal i, FloatVal f -> 
      if f = 0.0 then raise DivisionByZero
      else FloatVal (float_of_int i /. f)
  | FloatVal f, IntVal i -> 
      if i = 0 then raise DivisionByZero
      else FloatVal (f /. float_of_int i)
  | _ -> raise (TypeMismatch "Cannot divide non-numeric values")

let rem_values v1 v2 = 
  match v1, v2 with
  | IntVal i1, IntVal i2 -> 
      if i2 = 0 then raise DivisionByZero
      else IntVal (i1 mod i2)
  | _ -> raise (TypeMismatch "Modulo operation requires integers")

let pow_values v1 v2 = 
  let f1 = to_float v1 in
  let f2 = to_float v2 in
  FloatVal (f1 ** f2)

(* Vector operations *)
let vector_magnitude vec = 
  let sum_squares = Array.fold_left (fun acc v -> 
    let f = to_float v in acc +. (f *. f)) 0.0 vec in
  FloatVal (sqrt sum_squares)

let vector_dimension vec = IntVal (Array.length vec)

let vector_transpose vec = 
  Array.map (fun v -> [|v|]) vec

(* Matrix operations *)
let matrix_determinant mat = 
  let n = Array.length mat in
  if n = 0 || Array.length mat.(0) <> n then
    raise (RuntimeError "Determinant requires square matrix")
  else if n = 1 then
    mat.(0).(0)
  else if n = 2 then
    let a = to_float mat.(0).(0) in
    let b = to_float mat.(0).(1) in
    let c = to_float mat.(1).(0) in
    let d = to_float mat.(1).(1) in
    FloatVal (a *. d -. b *. c)
  else
    raise (RuntimeError "Determinant calculation only implemented for 1x1 and 2x2 matrices")

let matrix_transpose mat = 
  let rows = Array.length mat in
  if rows = 0 then [||] else
  let cols = Array.length mat.(0) in
  Array.init cols (fun j -> Array.init rows (fun i -> mat.(i).(j)))

(* Environment operations *)
let lookup_var env name = 
  try List.assoc name env
  with Not_found -> raise (UndefinedVariable name)

let bind_var env name value = (name, value) :: env

(* Main evaluation function *)
let rec eval env = function
  | Main ast -> eval env ast
  | Statement_list stmts -> eval_statements env stmts
  | EmptyStatement -> UnitVal
  | LineStatement ast -> eval env ast
  | BlockStatement ast -> eval env ast
  | ConditionalStatement ast -> eval env ast
  | LoopStatement ast -> eval env ast
  | Block ast -> eval env ast
  | Line ast -> eval env ast
  
  (* Arithmetic operations *)
  | Add (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      add_values v1 v2
  | Sub (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      sub_values v1 v2
  | Mul (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      mul_values v1 v2
  | Div (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      div_values v1 v2
  | Rem (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      rem_values v1 v2
  | Exponent (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      pow_values v1 v2
  | Neg e -> 
      let v = eval env e in
      (match v with
       | IntVal i -> IntVal (-i)
       | FloatVal f -> FloatVal (-.f)
       | _ -> raise (TypeMismatch "Cannot negate non-numeric value"))
  | Parenthesize e -> eval env e
  | Absolute e -> 
      let v = eval env e in
      (match v with
       | IntVal i -> IntVal (abs i)
       | FloatVal f -> FloatVal (abs_float f)
       | _ -> raise (TypeMismatch "Absolute value requires numeric input"))
  
  (* Boolean operations *)
  | BoolOr (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      BoolVal (to_bool v1 || to_bool v2)
  | BoolAnd (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      BoolVal (to_bool v1 && to_bool v2)
  | BoolNot e -> 
      let v = eval env e in
      BoolVal (not (to_bool v))
  
  (* Comparison operations *)
  | Equality (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      BoolVal (compare_values v1 v2 = 0)
  | NEquality (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      BoolVal (compare_values v1 v2 <> 0)
  | Greater (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      BoolVal (compare_values v1 v2 > 0)
  | Less (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      BoolVal (compare_values v1 v2 < 0)
  | GEquality (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      BoolVal (compare_values v1 v2 >= 0)
  | LEquality (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      BoolVal (compare_values v1 v2 <= 0)
  
  (* Constants *)
  | IntConst i -> IntVal i
  | FloatConst f -> FloatVal f
  | BoolConst b -> BoolVal b
  
  (* Variable operations *)
  | Lookup name -> lookup_var env name
  | Assignment (name, e) -> 
      let v = eval env e in
      let new_env = bind_var env name v in
      v
  
  (* Vector operations *)
  | Vector (Explist exprs) -> 
      let values = List.map (eval env) exprs in
      VectorVal (Array.of_list values)
  | VectorLookup (name, idx_expr) -> 
      let vec = lookup_var env name in
      let idx = to_int (eval env idx_expr) in
      (match vec with
       | VectorVal arr -> 
           if idx >= 0 && idx < Array.length arr then
             arr.(idx)
           else raise IndexOutOfBounds
       | _ -> raise (TypeMismatch "Variable is not a vector"))
  | VectorElemAssignment (name, idx_expr, val_expr) -> 
      let vec = lookup_var env name in
      let idx = to_int (eval env idx_expr) in
      let new_val = eval env val_expr in
      (match vec with
       | VectorVal arr -> 
           if idx >= 0 && idx < Array.length arr then (
             arr.(idx) <- new_val;
             new_val
           ) else raise IndexOutOfBounds
       | _ -> raise (TypeMismatch "Variable is not a vector"))
  
  (* Matrix operations *)
  | Matrix (Vlist vectors) -> 
      let vector_arrays = List.map (fun v -> 
        match eval env v with
        | VectorVal arr -> arr
        | _ -> raise (TypeMismatch "Matrix elements must be vectors")) vectors in
      MatrixVal (Array.of_list vector_arrays)
  | MatrixLookup (name, row_expr, col_expr) -> 
      let mat = lookup_var env name in
      let row = to_int (eval env row_expr) in
      let col = to_int (eval env col_expr) in
      (match mat with
       | MatrixVal arr -> 
           if row >= 0 && row < Array.length arr && 
              col >= 0 && col < Array.length arr.(row) then
             arr.(row).(col)
           else raise IndexOutOfBounds
       | _ -> raise (TypeMismatch "Variable is not a matrix"))
  
  (* Vector/Matrix specific operations *)
  | Magnitude e -> 
      let v = eval env e in
      (match v with
       | VectorVal arr -> vector_magnitude arr
       | _ -> raise (TypeMismatch "Magnitude requires a vector"))
  | Dimension e -> 
      let v = eval env e in
      (match v with
       | VectorVal arr -> vector_dimension arr
       | MatrixVal arr -> IntVal (Array.length arr)
       | _ -> raise (TypeMismatch "Dimension requires a vector or matrix"))
  | Transpose e -> 
      let v = eval env e in
      (match v with
       | VectorVal arr -> MatrixVal (vector_transpose arr)
       | MatrixVal arr -> MatrixVal (matrix_transpose arr)
       | _ -> raise (TypeMismatch "Transpose requires a vector or matrix"))
  | Determinant e -> 
      let v = eval env e in
      (match v with
       | MatrixVal arr -> matrix_determinant arr
       | _ -> raise (TypeMismatch "Determinant requires a matrix"))
  | Angle (e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VectorVal arr1, VectorVal arr2 -> calculate_angle arr1 arr2
       | _ -> raise (TypeMismatch "Angle calculation requires two vectors"))
  
  (* Control flow *)
  | IfElse (cond, then_stmt, else_stmt) -> 
      let cond_val = eval env cond in
      if to_bool cond_val then
        eval env then_stmt
      else
        eval env else_stmt
  | While (cond, body) -> 
      eval_while env cond body
  | For (init, cond, update, body) -> 
      eval_for env init cond update body
  
  (* I/O operations *)
  | Print msg -> 
      print_endline msg;
      flush stdout;
      UnitVal
  | TerminalInput -> 
      let input = read_line () in
      try IntVal (int_of_string input)
      with _ -> 
        try FloatVal (float_of_string input)
        with _ -> raise (RuntimeError "Invalid input format")
  
  (* Declarations *)
  | SingleDeclaration (typ, name) -> UnitVal
  | VectorDeclaration (typ, name, size_expr) -> 
      let size = to_int (eval env size_expr) in
      let default_val = default_value_for_type typ in
      let arr = Array.make size default_val in
      VectorVal arr
  | MatrixDeclaration (typ, name, rows_expr, cols_expr) -> 
      let rows = to_int (eval env rows_expr) in
      let cols = to_int (eval env cols_expr) in
      let default_val = default_value_for_type typ in
      let arr = Array.make_matrix rows cols default_val in
      MatrixVal arr
  
  | _ -> raise (RuntimeError "Unimplemented AST node")

and eval_statements env = function
  | [] -> UnitVal
  | [stmt] -> eval env stmt
  | stmt :: rest -> 
      let _ = eval env stmt in
      eval_statements env rest

and eval_while env cond body = 
  let rec loop () = 
    let cond_val = eval env cond in
    if to_bool cond_val then (
      let _ = eval env body in
      loop ()
    ) else UnitVal
  in loop ()

and eval_for env init cond update body = 
  let _ = eval env init in
  let rec loop () = 
    let cond_val = eval env cond in
    if to_bool cond_val then (
      let _ = eval env body in
      let _ = eval env update in
      loop ()
    ) else UnitVal
  in loop ()

and compare_values v1 v2 = 
  match v1, v2 with
  | IntVal i1, IntVal i2 -> compare i1 i2
  | FloatVal f1, FloatVal f2 -> compare f1 f2
  | BoolVal b1, BoolVal b2 -> compare b1 b2
  | IntVal i, FloatVal f -> compare (float_of_int i) f
  | FloatVal f, IntVal i -> compare f (float_of_int i)
  | _ -> raise (TypeMismatch "Cannot compare these value types")

and default_value_for_type = function
  | TypeInt -> IntVal 0
  | TypeFloat -> FloatVal 0.0
  | TypeBool -> BoolVal false

and calculate_angle arr1 arr2 = 
  if Array.length arr1 <> Array.length arr2 then
    raise (RuntimeError "Vectors must have same dimension for angle calculation")
  else
    let dot_product = ref 0.0 in
    let mag1_sq = ref 0.0 in
    let mag2_sq = ref 0.0 in
    for i = 0 to Array.length arr1 - 1 do
      let v1 = to_float arr1.(i) in
      let v2 = to_float arr2.(i) in
      dot_product := !dot_product +. (v1 *. v2);
      mag1_sq := !mag1_sq +. (v1 *. v1);
      mag2_sq := !mag2_sq +. (v2 *. v2);
    done;
    let mag1 = sqrt !mag1_sq in
    let mag2 = sqrt !mag2_sq in
    if mag1 = 0.0 || mag2 = 0.0 then
      raise (RuntimeError "Cannot calculate angle with zero vector")
    else
      FloatVal (acos (!dot_product /. (mag1 *. mag2)))

(* Main interpreter function *)
let interpret ast = 
  try
    let result = eval [] ast in
    Some result
  with
  | RuntimeError msg -> 
      Printf.eprintf "Runtime Error: %s\n" msg;
      None
  | TypeMismatch msg -> 
      Printf.eprintf "Type Error: %s\n" msg;
      None
  | DivisionByZero -> 
      Printf.eprintf "Error: Division by zero\n";
      None
  | IndexOutOfBounds -> 
      Printf.eprintf "Error: Index out of bounds\n";
      None
  | UndefinedVariable name -> 
      Printf.eprintf "Error: Undefined variable '%s'\n" name;
      None
