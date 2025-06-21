{
        open Tokens
        open Parser
        exception Error of string
}

let digit=['0'-'9']
let integer=digit+
let float_=digit* '.' digit+
let scientific=(float_ | integer) ('E'| 'e') ['+' '-']? (integer)
let letter=['a'-'z' 'A'-'Z' '_' ''']
let alphanumeric=letter | digit
let identifier=letter alphanumeric*
let inline_comment='#' [^'\n']* ('\n' | eof)
let comment_block='`' [^'`']* '`'
let string_=(alphanumeric | [ '?' '"' '/' '\\' '*' '<' '>' '|' '.'])*
let neq= "=/="
let gteq= ">="
let lteq= "<="
let abs_open="|("
let abs_close=")|"
let assign=":="
rule tokenize = parse
        | comment_block { 
                let comment = Lexing.lexeme lexbuf in
                String.iter (fun c -> if c = '\n' then Lexing.new_line lexbuf) comment;
                COMMENT_BLOCK 
        }
        | inline_comment { INLINE_COMMENT }
        | [ ' ' '\t' ] { tokenize lexbuf }
        | [ '\n' ] { 
                Lexing.new_line lexbuf;  
                tokenize lexbuf 
        }     
        | ("Print(" (string_ as temp)")") { PRINT (temp) }
        | ("Input(" (string_ as temp)")") { INPUT (temp) }
        | identifier as j {
                match j with

                | "int" -> TYPE_INT
                | "float" -> TYPE_FLOAT
                | "bool" -> TYPE_BOOL
                | "for" -> FOR
                | "if" -> IF
                | "else" -> ELSE
                | "while" -> WHILE
                | "mod" -> REM
                | "Turquoise" -> BOOL true
                | "Fuchsia" -> BOOL false
                | "vector" -> TYPE_VECTOR
                | "matrix" -> TYPE_MATRIX
                | "angle" -> ANGLE
                | "mag" -> MAGNITUDE
                | "dim" -> DIMENSION
                | "transpose" -> TRANSPOSE
                | "det" -> DETERMINANT
                | x -> VARS(x)
        }
        | neq { NEQ }
        | [ '=' ] { EQ }        
        | [ '>' ] { GT }
        | gteq { GTEQ }
        | [ '<' ] { LT }
        | lteq { LTEQ }
        | [ '|' ] { BOOL_OR }
        | abs_open { ABS_OPEN }
        | [ ')' ] { RIGHT_PAREN }
        | abs_close { ABS_CLOSE }
        | [ '^' ] { EXPONENT }
        | ['&']  { BOOL_AND }
        | ['~'] { BOOL_NOT }
        | ['+']  { ADD }
        | ['-']  { SUB }
        | ['*']  { MUL }
        | ['/']  { DIV }
        | ['{']  { LEFT_BRACE }
        | ['}']  { RIGHT_BRACE }
        | ['[']  { LEFT_SQUARE_BRACKET }
        | [']']  { RIGHT_SQUARE_BRACKET }
        | ['(']  { LEFT_PAREN }
        | [',']  { DELIMITER }
        | [';']  { SEMICOLON } 
        | assign { ASSIGN } 
        | scientific as s { FLOAT(float_of_string s) }
        | integer as i { INT(int_of_string i) } 
        | float_ as fl { FLOAT(float_of_string fl) }
        |  eof { EOF }
        | _ { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
