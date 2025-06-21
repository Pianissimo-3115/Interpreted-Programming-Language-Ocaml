open Ast
exception TypeError of ast * string
exception DimensionError of ast * string
exception VariableNotFound of string * string
exception OverwriteError of string * string
exception ScopeNotFound of ast * string;;

type type_ = 
	| Int
	| Float
	| Bool
	| VECTOR of type_ * ast
	| MATRIX of type_ * ast * ast
	| Void

let rec string_of_type x =		(*FOR DEBUGGING*)
	match x with
	| Int -> "Int"
	| Float -> "Float"
	| Bool -> "Bool"
	| VECTOR (t, n) -> Printf.sprintf "VECTOR(%s, %s)" (string_of_type t) (print_ast n)
	| MATRIX (t, n, m) -> Printf.sprintf "MATRIX(%s, %s, %s)" (string_of_type t) (print_ast n) (print_ast m)
	| Void -> "Void"

let g : (string, type_) Hashtbl.t list= []
let typeconv x =
	match x with
	| TypeInt -> Int
	| TypeBool -> Bool
	| TypeFloat -> Float
let rec lookup g str =
	match g with
	| [] -> raise (VariableNotFound (str,Printf.sprintf "Variable '%s' not defined in this scope" str))
	| table :: rest ->
		try Hashtbl.find table str
		with Not_found -> lookup rest str;;

let rec vectorConstCheck x =
	match x with
	| [] -> Void
	| [y] -> type_checker y g
	| y::ys -> 
		let fstt = type_checker y g in
		let sndd = vectorConstCheck ys in
		if fstt=sndd then fstt
		else if fstt=Float && sndd = Int then Float
		else if fstt=Int && sndd = Float then Float
		else Void

and matrixConstCheck x =
	match x with
	| [] -> 
			Void
	| [v] ->
		let result1 =
			match v with
			| Vector(Explist(y)) -> vectorConstCheck y
			| _ -> 
				Void
		in result1

	| v::vs ->
		let result1 =
			match v with
			| Vector(Explist(y)) -> vectorConstCheck y
			| _ -> 
				Void
		in
		let result2 = matrixConstCheck vs in
		if result1 = result2 then result1
		else if result1=Float && result2= Int then Float
		else if result1=Int && result2=Float then Float
		else 
			Void


and type_checker (astt : Ast.ast) g : type_ = 
	match astt with
	| Main statement_list ->
		let _ = type_checker statement_list g in
		Void

	| Statement_list xs -> 
		let _ = List.iter (fun ast -> ignore (type_checker ast g)) xs in
		Void
	| EmptyStatement -> Void
	| LineStatement x ->
		let _ = type_checker x g in
		Void
	| BlockStatement x -> 
		let _ = type_checker x g in
		Void
	| ConditionalStatement x -> 
		let _ = type_checker x g in
		Void
	| LoopStatement x -> 
		let _ = type_checker x g in
		Void
	
	| Block statement_list -> 
		let h : (string,type_) Hashtbl.t = Hashtbl.create 11 in
		let gg : (string,type_) Hashtbl.t list = h::g in
		let _ = type_checker statement_list gg in
		Void
	| Line x -> 
		let _ = type_checker x g in
		Void

	| Add (x, y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type, y_type) with
		| (Int, Int) -> Int
		| (Float, Int) -> Float
		| (Int, Float) -> Float
		| (Float, Float) -> Float
		| (VECTOR (Int, n), VECTOR (Int, m)) ->
			if n = m then VECTOR (Int, n)
			else raise (DimensionError (Add(x,y), "Invalid Dimension for addition"))
		| (VECTOR (Float, n), VECTOR (Float, m)) ->
			if n = m then VECTOR (Float, n)
			else raise (DimensionError (Add(x,y), "Invalid Dimension for addition"))
		| (VECTOR (Int, n), VECTOR (Float, m)) ->
			if n = m then VECTOR (Float, n)
			else raise (DimensionError (Add(x,y), "Invalid Dimension for addition"))
		| (VECTOR (Float, n), VECTOR (Int, m)) ->
			if n = m then VECTOR (Float, n)
			else raise (DimensionError (Add(x,y), "Invalid Dimension for addition"))
		| (MATRIX (Int, n1, m1), MATRIX (Int, n2, m2)) ->
			if n1=n2 && m1=m2 then MATRIX (Int, n1, m1)
			else raise (DimensionError (Add(x,y), "Invalid Dimension for addition"))
		| (MATRIX (Float, n1, m1), MATRIX (Float, n2, m2)) ->
			if n1=n2 && m1=m2 then MATRIX (Float, n1, m1)
			else raise (DimensionError (Add(x,y), "Invalid Dimension for addition"))
		| (MATRIX (Int, n, m), MATRIX (Float, p, q)) ->
			if m = p then MATRIX (Float, n, q)
			else raise (DimensionError (Add(x,y), "Invalid Dimension for addition"))
		| (MATRIX (Float, n, m), MATRIX (Int, p, q)) ->
			if m = p then MATRIX (Float, n, q)
			else raise (DimensionError (Add(x,y), "Invalid Dimension for addition"))
		| _ -> raise (TypeError (Add(x,y),"Invalid type for addition" ) )
		in result

	| Mul (x, y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type, y_type) with
		| (Int, Int) -> Int
		| (Float, Float) -> Float
		| (Float, Int) -> Float
		| (Int, Float) -> Float
		| (Int, VECTOR (Int, n)) -> VECTOR (Int, n)
		| (VECTOR (Int, n), Int) -> VECTOR (Int, n)
		| (Float, VECTOR (Float, n)) -> VECTOR (Float, n)
		| (VECTOR (Float, n), Float) -> VECTOR (Float, n)
		| (Int, VECTOR (Float, n)) -> VECTOR (Float, n)
		| (Float, VECTOR (Int, n)) -> VECTOR (Float, n)
		| (VECTOR (Float, n), Int) -> VECTOR (Float, n)
		| (VECTOR (Int, n), Float) -> VECTOR (Float, n)
		| (VECTOR (Int, n), VECTOR (Int, m)) ->
			if n = m then Int
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for dot product"))
		| (VECTOR (Float, n), VECTOR (Float, m)) ->
			if n = m then Float
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for dot product"))
		| (VECTOR (Int, n), VECTOR (Float, m)) ->
			if n = m then Float
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for dot product"))
		| (VECTOR (Float, n), VECTOR (Int, m)) ->
			if n = m then Float
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for dot product"))
		| (MATRIX (Int, n, m), MATRIX (Int, p, q)) ->
			if m = p then MATRIX (Int, n, q)
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for matrix product"))
		| (MATRIX (Float, n, m), MATRIX (Float, p, q)) ->
			if m = p then MATRIX (Float, n, q)
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for matrix product"))
		| (MATRIX (Int, n, m), MATRIX (Float, p, q)) ->
			if m = p then MATRIX (Float, n, q)
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for matrix product"))
		| (MATRIX (Float, n, m), MATRIX (Int, p, q)) ->
			if m = p then MATRIX (Float, n, q)
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for matrix product"))
		| (Int, MATRIX (Int, n, m)) -> MATRIX (Int, n, m)
		| (MATRIX (Int, n, m), Int) -> MATRIX (Int, n, m)
		| (Float, MATRIX (Float, n, m)) -> MATRIX (Float, n, m)
		| (MATRIX (Float, n, m), Float) -> MATRIX (Float, n, m)
		| (Int, MATRIX (Float, n, m)) -> MATRIX (Float, n, m)
		| (Float, MATRIX (Int, n, m)) -> MATRIX (Float, n, m)
		| (MATRIX (Float, n, m), Int) -> MATRIX (Float, n, m)
		| (MATRIX (Int, n, m), Float) -> MATRIX (Float, n, m)
		| (MATRIX (Int, n, m), VECTOR (Int, p)) ->
			if m = p then VECTOR (Int, n)
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for vector transformation"))
		| (MATRIX (Float, n, m), VECTOR (Float, p)) ->
			if m = p then VECTOR (Float, n)
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for vector transformation"))
		| (MATRIX (Int, n, m), VECTOR (Float, p)) ->
			if m = p then VECTOR (Float, n)
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for vector transformation"))
		| (MATRIX (Float, n, m), VECTOR (Int, p)) ->
			if m = p then VECTOR (Float, n)
			else raise (DimensionError (Mul(x,y), "Invalid Dimension for vector transformation"))
		| _ -> raise (TypeError (Mul(x,y), "Invalid type for multiplication"))

		in result

	| Sub (x, y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type, y_type) with
		| (Int, Int) -> Int
		| (Float, Int) -> Float
		| (Int, Float) -> Float
		| (Float, Float) -> Float
		| (VECTOR (Int, n), VECTOR (Int, m)) ->
			if n = m then VECTOR (Int, n)
			else raise (DimensionError (Sub(x,y), "Invalid Dimension for subtraction"))
		| (VECTOR (Float, n), VECTOR (Float, m)) ->
			if n = m then VECTOR (Float, n)
			else raise (DimensionError (Sub(x,y), "Invalid Dimension for subtraction"))
		| (VECTOR (Int, n), VECTOR (Float, m)) ->
			if n = m then VECTOR (Float, n)
			else raise (DimensionError (Sub(x,y), "Invalid Dimension for subtraction"))
		| (VECTOR (Float, n), VECTOR (Int, m)) ->
			if n = m then VECTOR (Float, n)
			else raise (DimensionError (Sub(x,y), "Invalid Dimension for subtraction"))
		| (MATRIX (Int, n1, m1), MATRIX (Int, n2, m2)) ->
			if n1=n2 && m1=m2 then MATRIX (Int, n1, m1)
			else raise (DimensionError (Sub(x,y), "Invalid Dimension for subtraction"))
		| (MATRIX (Float, n1, m1), MATRIX (Float, n2, m2)) ->
			if n1=n2 && m1=m2 then MATRIX (Float, n1, m1)
			else raise (DimensionError (Sub(x,y), "Invalid Dimension for subtraction"))
		| (MATRIX (Int, n, m), MATRIX (Float, p, q)) ->
			if m = p then MATRIX (Float, n, q)
			else raise (DimensionError (Sub(x,y), "Invalid Dimension for subtraction"))
		| (MATRIX (Float, n, m), MATRIX (Int, p, q)) ->
			if m = p then MATRIX (Float, n, q)
			else raise (DimensionError (Sub(x,y), "Invalid Dimension for subtraction"))
		| _ -> raise (TypeError (
			Sub(x,y),"Invalid type for subtraction" ) )
		
		in result

	| Div (x, y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type, y_type) with
		| (Int, Int) -> Int
		| (Float, Int) -> Float
		| (Int, Float) -> Float
		| (Float, Float) -> Float
		| _ -> raise (TypeError (Div(x,y), "Invalid type for division"))
		
		in result

	| Exponent (x, y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type, y_type) with
		| (Int, Int) -> Int
		| (Float, Int) -> Float
		| (Int, Float) -> Float
		| (Float, Float) -> Float
		| _ -> raise (TypeError (Exponent(x,y), "Invalid type for exponentiation"))
		in result

	| Rem (x, y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type, y_type) with
		| (Int, Int) -> Int
		| (Float, Int) -> Float
		| (Int, Float) -> Float
		| (Float, Float) -> Float
		| _ -> raise (TypeError (Rem(x,y), "Invalid type for remainder"))
		in result

	| Neg(x) ->
		let x_type = type_checker x g in
		let result =
		match x_type with
		| Int -> Int
		| Float -> Float
		| VECTOR (Int, n) -> VECTOR (Int, n)
		| VECTOR (Float, n) -> VECTOR (Float, n)
		| MATRIX (Int, n, m) -> MATRIX (Int, n, m)
		| MATRIX (Float, n, m) -> MATRIX (Float, n, m)
		| _ -> raise (TypeError (Neg(x), "Invalid type for negation"))
		in result

	| Parenthesize (x) -> type_checker x g
	| Dimension (x) -> 
		let x_type = type_checker x g in
		let result =
		match x_type with
		| VECTOR (Int, n) -> Int
		| VECTOR (Float, n) -> Int
		| MATRIX (Int, n, m) -> VECTOR(Int, IntConst 2)
		| MATRIX (Float, n, m) -> VECTOR(Int,IntConst 2)
		| _ -> raise (TypeError (Dimension(x), "Invalid type for Dimension"))
		in result

	| Magnitude (x) ->
		let x_type = type_checker x g in
		let result =
		match x_type with
		| VECTOR (Int, n) -> Float
		| VECTOR (Float, n) -> Float
		| _ -> raise (TypeError (Magnitude(x), "Invalid type for Magnitude"))
		in result

	| Angle (x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type, y_type) with
		| (VECTOR (Int, n), VECTOR (Int, m)) ->
			if n = m then Float
			else raise (DimensionError (Angle(x,y), "Invalid Dimension for angle"))
		| (VECTOR (Float, n), VECTOR (Float, m)) ->
			if n = m then Float
			else raise (DimensionError (Angle(x,y), "Invalid Dimension for angle"))
		| (VECTOR (Int, n), VECTOR (Float, m)) ->
			if n = m then Float
			else raise (DimensionError (Angle(x,y), "Invalid Dimension for angle"))
		| (VECTOR (Float, n), VECTOR (Int, m)) ->
			if n = m then Float
			else raise (DimensionError (Angle(x,y), "Invalid Dimension for angle"))
		| _ -> raise (TypeError (Angle(x,y), "Invalid type for angle"))

		in result
	| Determinant (x) ->
		let x_type = type_checker x g in
		let result =
		match x_type with
		| MATRIX (Int, n, m) -> Int
		| MATRIX (Float, n, m) -> Float
		| _ -> raise (TypeError (Determinant(x), "Invalid type for determinant"))
		in result
	| BoolOr(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type,y_type) with
		| (Bool, Bool) -> Bool
		| _ -> raise (TypeError (BoolOr(x,y), "Invalid type for boolean or"))
		in result
	| BoolAnd(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		match (x_type,y_type) with
		| (Bool, Bool) -> Bool
		| _ -> raise (TypeError (BoolAnd(x,y), "Invalid type for boolean and"))
		in result

	| BoolNot(x) ->
		let x_type = type_checker x g in
		let result =
		match x_type with
		| Bool -> Bool
		| _ -> raise (TypeError (BoolNot(x), "Invalid type for boolean not"))
		in result
	| Equality(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		if x_type = y_type then Bool
		else match (x_type,y_type) with
		| (VECTOR(t1,n1),VECTOR(t2,n2)) -> 
			if n1 = n2 then Bool
			else raise (DimensionError (Equality(x,y), "Dimension mismatch in comparison"))
		| (MATRIX(t1,n1,m1),MATRIX(t2,n2,m2)) ->
			if ((n1 = n2) && (m1 = m2)) then Bool
			else raise (DimensionError (Equality(x,y), "Dimension mismatch in comparison"))
		| _ -> raise (TypeError(Equality(x,y), "Type mismatch in comparison"))
	
		in result

	| NEquality(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		if x_type = y_type then Bool
		else match (x_type,y_type) with
		| (VECTOR(t1,n1),VECTOR(t2,n2)) -> 
			if n1 = n2 then Bool
			else raise (DimensionError (NEquality(x,y), "Dimension mismatch in comparison"))
		| (MATRIX(t1,n1,m1),MATRIX(t2,n2,m2)) ->
			if n1 = n2 && m1 = m2 then Bool
			else raise (DimensionError (NEquality(x,y), "Dimension mismatch in comparison"))
		| _ -> raise (TypeError(NEquality(x,y), "Type mismatch in comparison"))
		in result

	| Greater(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		if x_type = y_type then Bool
		else match (x_type,y_type) with
		| (VECTOR(t1,n1),VECTOR(t2,n2)) -> 
			if n1 = n2 then Bool
			else raise (DimensionError (Greater(x,y), "Dimension mismatch in comparison"))
		| (MATRIX(t1,n1,m1),MATRIX(t2,n2,m2)) ->
			if n1 = n2 && m1 = m2 then Bool
			else raise (DimensionError (Greater(x,y), "Dimension mismatch in comparison"))
		| _ -> raise (TypeError(Greater(x,y), "Type mismatch in comparison"))
		in result
	| Less(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		if x_type = y_type then Bool
		else match (x_type,y_type) with
		| (VECTOR(t1,n1),VECTOR(t2,n2)) -> 
			if n1 = n2 then Bool
			else raise (DimensionError (Less(x,y), "Dimension mismatch in comparison"))
		| (MATRIX(t1,n1,m1),MATRIX(t2,n2,m2)) ->
			if n1 = n2 && m1 = m2 then Bool
			else raise (DimensionError (Less(x,y), "Dimension mismatch in comparison"))
		| _ -> raise (TypeError(Less(x,y), "Type mismatch in comparison"))
		in result
	| GEquality(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		if x_type = y_type then Bool
		else match (x_type,y_type) with
		| (VECTOR(t1,n1),VECTOR(t2,n2)) -> 
			if n1 = n2 then Bool
			else raise (DimensionError (GEquality(x,y), "Dimension mismatch in comparison"))
		| (MATRIX(t1,n1,m1),MATRIX(t2,n2,m2)) ->
			if n1 = n2 && m1 = m2 then Bool
			else raise (DimensionError (GEquality(x,y), "Dimension mismatch in comparison"))
		| _ -> raise (TypeError(GEquality(x,y), "Type mismatch in comparison"))
		in result
	| LEquality(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
		if x_type = y_type then Bool
		else match (x_type,y_type) with
		| (VECTOR(t1,n1),VECTOR(t2,n2)) -> 
			if n1 = n2 then Bool
			else raise (DimensionError (LEquality(x,y), "Dimension mismatch in comparison"))
		| (MATRIX(t1,n1,m1),MATRIX(t2,n2,m2)) ->
			if n1 = n2 && m1 = m2 then Bool
			else raise (DimensionError (LEquality(x,y), "Dimension mismatch in comparison"))
		| _ -> raise (TypeError(LEquality(x,y), "Type mismatch in comparison"))
		in result
	| Absolute(x) ->
		let x_type = type_checker x g in
		let result =
		match x_type with
		| Int | Float | VECTOR(_,_) | MATRIX(_,_,_) -> x_type
		| _ -> raise (TypeError(Absolute(x), "Invalid type for Absolute value"))
		in result

	| Transpose(x) ->
		let x_type = type_checker x g in
		let result =
		match x_type with
		| MATRIX (Int, n, m) -> MATRIX (Int, m, n)
		| MATRIX (Float, n, m) -> MATRIX (Float, m, n)
		| _ -> raise (TypeError (Transpose(x), "Invalid type for transpose"))
		in result

	| Lookup(x) ->
		lookup g x

	| VectorLookup(x,y) ->
		let x_type = lookup g x in
		let y_type = type_checker y g in
		let result =
		match (x_type,y_type) with
		| (VECTOR(t1,_),Int) -> t1
		| (_,_) -> raise (TypeError(VectorLookup(x,y),"Invalid type for Vector lookup"))
		in result

	| MatrixLookup(x,y,z) ->
		let x_type = lookup g x in
		let y_type = type_checker y g in
		let z_type = type_checker z g in
		let result =
		match (x_type,y_type,z_type) with
		| (MATRIX(t1,_,_),Int,Int) -> t1
		| (_,_,_) -> raise(TypeError(MatrixLookup(x,y,z),"Invalid type for Matrix lookup"))
		in result

	| IntConst(x) ->
		Int
	| FloatConst(x) ->
		Float
	| BoolConst(x) ->
		Bool
	| Vector(x) ->
		let result =
		match x with
		| Explist(y) ->
			let y_type = vectorConstCheck y in
			if y_type = Void then raise(TypeError(Vector(x),"Invalid type for Vector"))
			else VECTOR(y_type, IntConst (List.length y))
		| _ -> raise(TypeError(Vector(x), "Invalid type for Vector"))
		in result
	| Matrix(x) ->
		let result =
		match x with
		| Vlist(y) ->
			let y_type = matrixConstCheck y in
			if y_type = Void then begin
				raise(TypeError(Matrix(x),"Invalid type for Matrix"))
			end else
				let n = List.length y in
				let m = match List.hd y with
					| Vector(Explist(a)) -> List.length a
					| _ -> 
						raise(TypeError(Matrix(x), "Invalid type for Matrix")) 
				in				
				MATRIX(y_type,IntConst n,IntConst m)
		| _ -> 
			print_endline ("wowowow");
			raise(TypeError(Matrix(x), "Invalid type for Matrix"))
		in result
	| SingleDeclaration(x,y) ->
		let result =
		match g with
		| z::xs -> 
			let ans =
			try 
				let _ = Hashtbl.find z y in
				raise (OverwriteError(y,Printf.sprintf "Variable '%s' already exists in this scope" y))
			with Not_found -> let _ = (Hashtbl.add z y (typeconv x) ) in typeconv(x)

		in ans
		| _-> raise(ScopeNotFound(SingleDeclaration(x,y), "Scope not found for variable declaration"))

		in result

	| VectorDeclaration(x,y,z) ->
		let result =
			match g with
			| a::xs ->
				let ans =
				try
					let _ = Hashtbl.find a y in
					raise (OverwriteError(y,Printf.sprintf "Variable '%s' already exists in this scope" y))
				with Not_found -> 
					let z_type = type_checker z g in
					let new_result =
						match z_type with
						| Int -> 
							let ans =
							try let _ = Hashtbl.add a y (VECTOR(typeconv(x),z)) in VECTOR(typeconv(x),z)
							with _ -> raise(TypeError(VectorDeclaration(x,y,z),"Invalid Vector size"))
						in ans
						| _ -> raise(TypeError(VectorDeclaration(x,y,z),"Invalid type for Vector size"))
					in new_result
				in ans
			| _ -> raise(ScopeNotFound(VectorDeclaration(x,y,z), "Scope not found for variable declaration"))
		in result

	| MatrixDeclaration(x,y,z,w) ->
		let result =
			match g with
			| a::xs ->
				let anss =
				try
					let _ = Hashtbl.find a y in
					raise (OverwriteError(y,Printf.sprintf "Variable '%s' already exists in this scope" y))
				with Not_found -> 
					let z_type = type_checker z g in
					let w_type = type_checker w g in
					let new_result =
						match (z_type,w_type) with
						| (Int,Int) ->
							let ans =
							try let _ = Hashtbl.add a y (MATRIX(typeconv(x),z,w)) in MATRIX(typeconv(x),z,w)
							with _ -> raise(TypeError(MatrixDeclaration(x,y,z,w),"Invalid Matrix dimensions"))
						in ans
						| _ -> raise(TypeError(MatrixDeclaration(x,y,z,w),"Invalid type for Matrix dimensions"))

					in new_result
				in anss
			| _ -> raise(ScopeNotFound(MatrixDeclaration(x,y,z,w), "Scope not found for variable declaration"))
		in result

		| DeclAssignment(x, y) ->
			let x_type = type_checker x g in
			let y_type = type_checker y g in
			begin
				(* print_endline ("x_type: " ^ string_of_type x_type);
				print_endline ("y_type: " ^ string_of_type y_type); *)
				match (x_type, y_type) with
				| (Float, Int) -> Void
				| (VECTOR(t1,n1),VECTOR(t2,n2))->
					let result =
						if t1 = t2 then Void
						else if t1 = Float && t2 = Int then Void
						else raise (TypeError (DeclAssignment(x, y), "Type mismatch in variable assignment"))
					in 
					begin
						if n1 = n2 then result
						else raise (DimensionError (DeclAssignment (x, y), "Dimension mismatch in variable assignment"))
					end
				| (MATRIX(t1,n1,n2), MATRIX(t2,m1,m2)) ->
					let result =
						if t1=t2 then Void
						else if t1 = Float && t2 = Int then Void
						else raise (TypeError (DeclAssignment(x,y),"Type mismatch in variable assignment"))
					in 
					begin
						if n1 = m1 && n2 = m2 then result
						else raise (DimensionError (DeclAssignment (x, y), "Dimension mismatch in variable assignment"))
					end
				| (t1,t2) -> 
					if t1=t2 then Void
					else raise (TypeError (DeclAssignment(x, y), "Type mismatch in variable assignment"))
				end
	
	| DeclInpAssignment(x,y) ->
		let _ = type_checker x g in
		let y_type = type_checker y g in
		if y_type = Void then Void
		else raise(TypeError(DeclInpAssignment(x,y),"Invalid type in variable assignment"))

	| Assignment(x,y) ->
		let x_type = lookup g x in
		let y_type = type_checker y g in
		let result =
		match (x_type, y_type) with
		| (Float, Int) -> Void
		| (VECTOR(t1,n1),VECTOR(t2,n2))->
			let resultt =
				if t1 = t2 then Void
				else if t1 = Float && t2 = Int then Void
				else raise (TypeError (Assignment(x, y), "Type mismatch in variable assignment"))
			in 
			begin
				if n1 = n2 then resultt
				else raise (DimensionError (Assignment (x, y), "Dimension mismatch in variable assignment"))
			end
		| (MATRIX(t1,n1,n2), MATRIX(t2,m1,m2)) ->
			let resultt =
				if t1 = t2 then Void
				else if t1 = Float && t2 = Int then Void
				else raise (TypeError (Assignment(x,y),"Type mismatch in variable assignment"))
			in 
			begin
				if n1 = m1 && n2 = m2 then resultt
				else raise (DimensionError (Assignment (x, y), "Dimension mismatch in variable assignment"))
			end
		| (t1,t2) -> 
			if t1=t2 then Void
			else raise (TypeError (Assignment(x, y), "Type mismatch in variable assignment"))
		in result

	| InpAssignment(x,y) ->
		let _ = lookup g x in
		let y_type = type_checker y g in
		if y_type = Void then Void
		else raise(TypeError(InpAssignment(x,y),"Invalid type in variable assignment"))
	| VectorElemAssignment(x,y,z) ->
		let x_type = lookup g x in
		let y_type = type_checker y g in
		let z_type = type_checker z g in
		let result =
		match (x_type,y_type,z_type) with
		| (VECTOR(t1,n),Int,t2) ->
			if t1 = t2 then Void
			else if t1 = Float && t2 = Int then Void
			else raise(TypeError(VectorElemAssignment(x,y,z),"Type mismatch in Vector element assignment"))

		| (_,_,_) -> raise(TypeError(VectorElemAssignment(x,y,z),"Invalid type for Vector element assignment"))

		in result

	| VectorElemInpAssignment(x,y,z) ->
		let x_type = lookup g x in
		let y_type = type_checker y g in
		let z_type = type_checker z g in
		let result =
		match (x_type,y_type,z_type) with
		| (VECTOR(t1,n),Int,Void) ->
			Void
		| (_,_,_) -> raise(TypeError(VectorElemInpAssignment(x,y,z),"Invalid type for Vector element assignment"))
		in result

	| MatrixElemAssignment(x,y,z,w) ->
		let x_type = lookup g x in
		let y_type = type_checker y g in
		let z_type = type_checker z g in
		let w_type = type_checker w g in
		let result =
		match (x_type,y_type,z_type,w_type) with
		| (MATRIX(t1,n,m),Int,Int,t2) ->
			if t1 = t2 then Void
			else if t1 = Float && t2 = Int then Void
			else raise(TypeError(MatrixElemAssignment(x,y,z,w),"Type mismatch in Matrix element assignment"))
		| (_,_,_,_) -> raise(TypeError(MatrixElemAssignment(x,y,z,w),"Invalid type for Matrix element assignment"))
		in result

	| MatrixElemInpAssignment(x,y,z,w) ->
		let x_type = lookup g x in
		let y_type = type_checker y g in
		let z_type = type_checker z g in
		let w_type = type_checker w g in
		let result =
			(match (x_type,y_type,z_type,w_type) with
			| (MATRIX(t1,n,m),Int,Int,Void) ->
				Void
			| (_,_,_,_) -> raise(TypeError(MatrixElemInpAssignment(x,y,z,w),"Invalid type for Matrix element assignment")))
			in result

	| Print(x) ->
		let _ = lookup g x in Void

	| TerminalInput -> Void
	| FileInput(x) -> Void
	| IfElse(x,y,z) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let z_type = type_checker z g in
		let result =
		match (x_type,y_type,z_type) with
		| (Bool,Void,Void) -> Void
		| (_,_,_) -> raise(TypeError(IfElse(x,y,z),"Invalid type for If-Else statement"))
		in result
	| For(x,y,z,w) ->
		let h : (string, type_) Hashtbl.t = Hashtbl.create 1 in
		let gg : (string, type_) Hashtbl.t list = h :: g in
		let x_type = type_checker x gg in
		let y_type = type_checker y gg in
		let z_type = type_checker z gg in
		let w_type = type_checker w gg in
		let result =
		match (x_type,y_type,z_type,w_type) with
		| (Void,Bool,Void,Void) -> Void
		| (_,_,_,_) -> raise(TypeError(For(x,y,z,w),"Invalid type for For loop"))
		in result
	| While(x,y) ->
		let x_type = type_checker x g in
		let y_type = type_checker y g in
		let result =
			match (x_type,y_type) with
			| (Bool,Void) -> Void
			| (_,_) -> raise(TypeError(While(x,y),"Invalid type for While loop"))
		in result
	| _ -> raise (TypeError(astt, "Invalid type for expression"));;


let main ast = type_checker ast g
