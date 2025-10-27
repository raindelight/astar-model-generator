parser grammar NewMinizincParser;

options { tokenVocab = NewMinizincLexer; }

model: (item SEMI)+;

item
    : include_item
    | var_decl_item
    | enum_item
    | type_inst_syn_item
    | assign_item
    | constraint_item
    | solve_item
    | output_item
    | predicate_item
    | test_item
    | function_item
    | annotation_item
    ;

ti_expr_and_id
    : ti_expr COLON ident
    | expr COLON ident
    ;

include_item: INCLUDE string_literal;

var_decl_item
    : ti_expr_and_id annotations (ASSIGN expr)?
    | ANY COLON ident annotations (ASSIGN expr)?
    ;

enum_item
    : ENUM_T ident annotations (ASSIGN enum_cases_list)?
    ;

enum_cases_list
    : enum_cases (DOUBLE_PLUS enum_cases)*
    ;

enum_cases
    : LBRACE ident (COMMA ident)* RBRACE
    | UNDERSCORE LPAREN expr RPAREN
    | ident LPAREN expr RPAREN
    | ANON_ENUM_T LPAREN expr RPAREN
    ;


type_inst_syn_item
    : TYPE ident annotations ASSIGN ti_expr
    ;

assign_item
    : ident ASSIGN expr
    ;

constraint_item
    : CONSTRAINT string_annotation expr
    ;

solve_item
    : SOLVE (annotations)? SATISFY
    | SOLVE (annotations)? MINIMIZE expr
    | SOLVE (annotations)? MAXIMIZE expr
    ;

output_item
    : OUTPUT string_annotation expr
    ;

annotation_item
    : ANNOTATION ident params (ASSIGN expr)?
    ;

function_item
    : FUNCTION ti_expr COLON ident params annotations (ASSIGN expr)?
    ;

predicate_item
    : PREDICATE ident params annotations (ASSIGN expr)?
    ;

test_item
    : TEST ident params annotations (ASSIGN expr)?
    ;

function_signature
    : ident params annotations
    ;


params
    : LPAREN (ti_expr_and_id (COMMA ti_expr_and_id)*)? RPAREN
    ;

ti_expr
    : base_ti_expr
    | array_ti_expr
    ;

base_ti_expr
   : var_par opt_ti set_ti base_ti_expr_tail (DOUBLE_PLUS base_ti_expr)?
   | expr
   | ANY ti_variable_expr_tail
   ;


var_par
    : VAR
    | PAR
    |
    ;
opt_ti
    : OPT
    |
    ;
set_ti
    : SET OF
    |
    ;

base_type
    : BOOL_TYPE
    | INT_TYPE
    | FLOAT_TYPE
    | STRING_TYPE
    ;

base_ti_expr_tail
    : ident
    | base_type
    | ti_variable_expr_tail
    | tuple_ti_expr_tail
    | record_ti_expr_tail
    | ANN
    | expr
    ;


ti_variable_expr_tail
    : IDENT
    ;

array_ti_expr
    : ARRAY LBRACKET ti_expr (COMMA ti_expr)* RBRACKET OF base_ti_expr
    | LIST OF base_ti_expr
    ;

tuple_ti_expr_tail
    : LPAREN (opt_ti? ti_expr) (COMMA (opt_ti? ti_expr))* RPAREN ;

record_ti_expr_tail
    : RECORD LPAREN ti_expr_and_id (COMMA ti_expr_and_id)* RPAREN
    ;

expr
    : expr_atom expr_binop_tail
    ;

expr_atom
    : expr_atom_head expr_atom_tail annotations
    ;

expr_binop_tail
    : (bin_op expr)?
    ;

expr_atom_head
    : builtin_un_op expr_atom
    | LPAREN expr RPAREN
    | ident_or_quoted_op
    | UNDERSCORE
    | bool_literal
    | int_literal
    | float_literal
    | string_literal
    | set_literal
    | set_comp
    | array_literal
    | array_literal_2d
    | indexed_array_literal
    | indexed_array_literal_2d
    | tuple_literal
    | record_literal
    | array_comp
    | indexed_array_comp
    | ann_literal
    | if_then_else_expr
    | let_expr
    | call_expr
    | gen_call_expr
    ;

expr_atom_tail
    :
    | array_access_tail expr_atom_tail
    | DOT (ident | INT_LITERAL) expr_atom_tail
    ;



num_expr
    : num_expr_atom num_expr_binop_tail
    ;
num_expr_atom
    : num_expr_atom_head expr_atom_tail annotations
    ;

num_expr_binop_tail
    : (num_bin_op num_expr) ?
    ;


num_expr_atom_head
    : builtin_num_un_op num_expr_atom
    | LPAREN num_expr RPAREN
    | ident_or_quoted_op
    | int_literal
    | MINUS int_literal
    | float_literal
    | MINUS float_literal
    | if_then_else_expr
    | let_expr
    | call_expr
    | gen_call_expr
    ;

builtin_op
    : builtin_bin_op
    | builtin_un_op
    ;

bin_op
    : builtin_bin_op
    | ident // `` ??
    ;

builtin_bin_op
    : AREQ
    | IMPL
    | REIMPL
    | AND_LOGIC
    | XOR
    | OR_LOGIC
    | LT
    | GT
    | LE
    | GE
    | EQ
    | ASSIGN
    | NE
    | TEQ
    | TNEQ
    | IN
    | SUBSET
    | SUPERSET
    | UNION
    | DIFF
    | SUPERSET
    | SYMDIFF
    | DOTDOT
    | INTERSECT
    | DOUBLE_PLUS
    | DEFAULT_T
    | builtin_num_bin_op
    ;

builtin_un_op
    : NOT
    | builtin_num_un_op
    ;

num_bin_op
   : builtin_num_bin_op
   | ident //`` ??
   ;

builtin_num_bin_op
    : PLUS
    | MINUS
    | MUL
    | DIV
    | DIV_WORD
    | MOD
    | POWER
    | TPLUS
    | TMINUS
    | TMUL
    | TDIV
    | TDIV_WORD
    ;
builtin_num_un_op
    : PLUS
    | MINUS
    ;

bool_literal
    : FALSE
    | TRUE
    ;

int_literal
    : INT_LITERAL;

float_literal
    : FLOAT_LITERAL;

string_literal
    : STRING_START (string_part)* STRING_END
    ;

string_part
    : STRING_TEXT
    | INTERP_START expr RPAREN
    ;
set_literal
    : LBRACE (expr (COMMA expr)*)? RBRACE (COLON ti_expr)?
    ;
set_comp
    : LBRACE expr PIPE comp_tail RBRACE
    ;


comp_tail: generator (WHERE expr)? (COMMA generator (WHERE expr)?)*;

generator: (ident | UNDERSCORE) (COMMA (ident | UNDERSCORE))* IN expr;

array_literal: LBRACKET (expr (COMMA expr)*)? RBRACKET;

array_literal_2d: LBAR (array_row (PIPE array_row)*)? RBAR;
array_row: expr (COMMA expr)*;

indexed_array_literal_2d: LBAR indexed_2d_row (PIPE indexed_2d_row)* RBAR;
indexed_2d_row
    : expr (COLON expr)*
    | (expr COLON)? expr (COMMA expr)*
    ;

indexed_array_literal
    : LBRACKET (indexed_item (COMMA indexed_item)* COMMA?)? RBRACKET
    | LBRACKET index_tuple COLON expr (COMMA expr)* COMMA? RBRACKET
    ;

indexed_item: index_tuple COLON expr;

index_tuple: expr | LPAREN (expr (COMMA expr)*)? RPAREN;

tuple_literal: LPAREN expr COMMA (expr (COMMA expr)*)? RPAREN;

record_literal: LPAREN record_item (COMMA record_item)* RPAREN;
record_item: ident COLON expr;

array_comp: LBRACKET expr PIPE comp_tail RBRACKET;

indexed_array_comp: LBRACKET index_tuple COLON expr PIPE comp_tail RBRACKET;

array_access_tail: LBRACKET expr (COMMA expr)* RBRACKET;

ann_literal: ident (LPAREN (expr (COMMA expr)*)? RPAREN)?;

if_then_else_expr: IF expr THEN expr (ELSEIF expr THEN expr)* (ELSE expr)? ENDIF;




call_expr: ident_or_quoted_op (LPAREN (expr (COMMA expr)*)? RPAREN)?;

let_expr
    : LET LBRACE let_item (SEMI let_item)* SEMI? RBRACE IN expr
    ;

let_item: var_decl_item | constraint_item;

gen_call_expr: ident_or_quoted_op LPAREN comp_tail RPAREN LPAREN expr RPAREN;

ident
    : IDENT
    | QUOTED_IDENT
    | SINGLE_QUOTED_IDENT
    ;

ident_or_quoted_op: ident;

annotations: (COLONCOLON annotation)*;

annotation: expr;

string_annotation: COLONCOLON string_literal;