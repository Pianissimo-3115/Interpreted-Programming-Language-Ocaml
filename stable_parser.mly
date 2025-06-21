%{
        open Ast
%}
%token ADD MUL SUB DIV EQ GT LT GTEQ LTEQ NEQ REM
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VARS
%token BOOL_AND BOOL_OR BOOL_NOT TYPE_INT TYPE_BOOL TYPE_FLOAT TYPE_VECTOR TYPE_MATRIX FOR WHILE EXPONENT IF ELSE
%token <string> PRINT
%token ANGLE MAGNITUDE DIMENSION TRANSPOSE DETERMINANT
%token <string> INPUT
%token ABS_OPEN ABS_CLOSE ASSIGN LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET SEMICOLON DELIMITER INLINE_COMMENT COMMENT_BLOCK EOF

%left BOOL_OR
%left BOOL_AND
%nonassoc BOOL_NOT

%left GT LT GTEQ LTEQ EQ NEQ

%left ADD SUB
%left MUL DIV REM
%nonassoc USUB
%right EXPONENT


%nonassoc MAGNITUDE DIMENSION DETERMINANT
%nonassoc TRANSPOSE

%start main
%type <Ast.ast> main
%%

main:
        | statement_list EOF
                {Main $1 }

statement_list:
  | statement
    { Statement_list [$1] }
  | statement statement_list
    { 
      match $2 with
      | Statement_list lst -> Statement_list ($1::lst) 
      | _ -> failwith "Critical parser error: Invalid statement_list structure" 
    }

statement:
        | line SEMICOLON
                {LineStatement $1}
        | block
                {BlockStatement $1}
        | conditional
                {ConditionalStatement $1 }
        | loop
                {LoopStatement $1 }
        | INLINE_COMMENT
                { Comment }
        | COMMENT_BLOCK
                { Comment }
block:
        | LEFT_BRACE statement_list RIGHT_BRACE
                {Block $2}
line:
        | expression
                {Line $1 }
        | declaration
                {Line $1 }
        | assignment
                {Line $1 }
        | interaction
                {Line $1 }

expression:
        | expression ADD expression { Add($1,$3)}
        | expression MUL expression  { Mul($1,$3)}
        | expression SUB expression { Sub($1,$3)}
        | expression DIV expression { Div($1,$3)}
        | expression EXPONENT expression { Exponent($1,$3)}
        | expression REM expression { Rem($1,$3)}
        | SUB expression %prec USUB { Neg $2}
        | LEFT_PAREN expression RIGHT_PAREN { Parenthesize $2}
        | DIMENSION expression { Dimension $2}
        | MAGNITUDE expression { Magnitude $2}
        | ANGLE LEFT_PAREN expression DELIMITER expression RIGHT_PAREN{ Angle($3,$5)}
        | DETERMINANT expression { Determinant $2}
        | expression BOOL_OR expression {BoolOr($1,$3)}
        | expression BOOL_AND expression {BoolAnd($1,$3)}
        | BOOL_NOT expression {BoolNot $2}
        | expression EQ expression {Equality($1,$3)}
        | expression NEQ expression {NEquality($1,$3)}
        | expression GT expression {Greater($1,$3)}
        | expression LT expression {Less($1,$3)}
        | expression GTEQ expression {GEquality($1,$3)}
        | expression LTEQ expression {LEquality($1,$3)}
        | ABS_OPEN expression ABS_CLOSE {Absolute $2}
        | TRANSPOSE expression {Transpose $2}
        | constant {$1}
        | VARS {Lookup $1}
        | VARS LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET {VectorLookup ($1,$3)}
        | VARS LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET {MatrixLookup($1,$3,$6)}

constant:
        | single {$1}
        | vector {$1}
        | matrix {$1}
single:
        | INT {IntConst $1}
        | FLOAT {FloatConst $1}
        | BOOL {BoolConst $1}

vector:
        | LEFT_SQUARE_BRACKET newexpressionlist RIGHT_SQUARE_BRACKET {Vector $2}

matrix:
        | LEFT_SQUARE_BRACKET vectorlist RIGHT_SQUARE_BRACKET {Matrix $2}

newexpression:
        | newexpression ADD newexpression { Add($1,$3)}
        | newexpression MUL newexpression  { Mul($1,$3)}
        | newexpression SUB newexpression { Sub($1,$3)}
        | newexpression DIV newexpression { Div($1,$3)}
        | newexpression EXPONENT newexpression { Exponent($1,$3)}
        | newexpression REM newexpression { Rem($1,$3)}
        | SUB newexpression { Neg $2}
        | LEFT_PAREN newexpression RIGHT_PAREN { Parenthesize $2}
        | newexpression BOOL_OR newexpression {BoolOr($1,$3)}
        | newexpression BOOL_AND newexpression {BoolAnd($1,$3)}
        | BOOL_NOT newexpression {BoolNot $2}
        | newexpression EQ newexpression {Equality($1,$3)}
        | newexpression NEQ newexpression {NEquality($1,$3)}
        | newexpression GT newexpression {Greater($1,$3)}
        | newexpression LT newexpression {Less($1,$3)}
        | newexpression GTEQ newexpression {GEquality($1,$3)}
        | newexpression LTEQ newexpression {LEquality($1,$3)}
        | ABS_OPEN newexpression ABS_CLOSE {Absolute $2}
        | newconstant {$1}
        | VARS {Lookup $1}
        | VARS LEFT_SQUARE_BRACKET newexpression RIGHT_SQUARE_BRACKET {VectorLookup($1,$3)}
        | VARS LEFT_SQUARE_BRACKET newexpression RIGHT_SQUARE_BRACKET LEFT_SQUARE_BRACKET newexpression RIGHT_SQUARE_BRACKET {MatrixLookup($1,$3,$6)}
newconstant:
        | single {$1}

newexpressionlist:
        | newexpression
                { Explist [$1] }
        | newexpression DELIMITER newexpressionlist
                {
                        match $3 with
                                | Explist temp -> Explist($1::temp)
                                | _ -> failwith "Critical parser error: Invalid expression structure" 
                }
vectorlist:
        | vector
                { Vlist [$1] }
        | vector DELIMITER vectorlist
               {
                        match $3 with
                                | Vlist temp -> Vlist($1::temp)
                                | _ ->failwith "Critical parser error: Invalid vector structure"
               }
                                
               



declaration:
        | onlydeclaration
                { $1}
        | declaration_assignment
                { $1}

onlydeclaration:
        | singletype VARS
                { SingleDeclaration($1,$2)}
        | TYPE_VECTOR LT singletype GT VARS LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET { VectorDeclaration($3,$5,$7)}
        | TYPE_MATRIX LT singletype GT VARS LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET {MatrixDeclaration($3,$5,$7,$10)}

declaration_assignment:
        | onlydeclaration ASSIGN expression
                { DeclAssignment($1,$3)}
        | onlydeclaration ASSIGN input
                {DeclInpAssignment($1,$3)}

assignment:
        | VARS ASSIGN expression
                { Assignment($1,$3)}
        | VARS ASSIGN input
                { InpAssignment($1,$3)}
        | VARS LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET ASSIGN expression
                { VectorElemAssignment($1,$3,$6)}
        | VARS LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET ASSIGN input 
                { VectorElemInpAssignment($1,$3,$6)}
        | VARS LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET ASSIGN expression
                { MatrixElemAssignment($1,$3,$6,$9)}
        | VARS LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET LEFT_SQUARE_BRACKET expression RIGHT_SQUARE_BRACKET ASSIGN input
                { MatrixElemInpAssignment($1,$3,$6,$9)}

interaction:
        | PRINT {Print $1}
        | input {$1}
input:
        | INPUT { 
                match $1 with
                      |  "" -> TerminalInput
                      |  x  -> FileInput x
          }              


conditional:
        | IF LEFT_PAREN expression RIGHT_PAREN statement ELSE statement
                { IfElse($3,$5,$7)}
loop:
        | FOR LEFT_PAREN declaration_assignment SEMICOLON expression SEMICOLON assignment RIGHT_PAREN statement
                { For($3,$5,$7,$9)}
        | FOR LEFT_PAREN assignment SEMICOLON expression SEMICOLON assignment RIGHT_PAREN statement
                { For($3,$5,$7,$9)}
               
        | WHILE LEFT_PAREN expression RIGHT_PAREN statement
                { While($3,$5)}

singletype:
        | TYPE_INT { TypeInt }
        | TYPE_BOOL { TypeBool }
        | TYPE_FLOAT { TypeFloat }














 
