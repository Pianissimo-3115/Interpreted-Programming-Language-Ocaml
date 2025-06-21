type tokens =
        | ADD
        | MUL
        | SUB
        | DIV
        | EQ
        | GT
        | LT
        | GTEQ
        | LTEQ
        | NEQ
        | REM
        | INT of int
        | FLOAT of float
        | BOOL of bool
        | VARS of string
        | BOOL_AND
        | BOOL_OR
        | BOOL_NOT
        | TYPE_INT
        | TYPE_BOOL
        | TYPE_FLOAT
        | TYPE_VECTOR
        | TYPE_MATRIX
        | FOR
        | WHILE
        | EXPONENT
        | IF
        | ELSE
        | PRINT of string
        | ANGLE
        | MAGNITUDE
        | DIMENSION
        | TRANSPOSE
        | DETERMINANT
        | INPUT of string
        | ABS_OPEN
        | ABS_CLOSE
        | ASSIGN
        | LEFT_PAREN
        | RIGHT_PAREN
        | LEFT_BRACE
        | RIGHT_BRACE
        | LEFT_SQUARE_BRACKET
        | RIGHT_SQUARE_BRACKET
        | SEMICOLON
        | DELIMITER
        | INLINE_COMMENT
        | COMMENT_BLOCK
        | EOF
 ;;

 exception Error of string
