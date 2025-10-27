grammar ModelConstraintGrammar;

goal: constraint EOF;

constraint:
   CONSTRAINT expr SEMI NL;

expr
    : LPAREN expr RPAREN
    | forall
    | exists
    | array_acces
    | array_range_acces
    | INT
    | unary_expr
    | implies_expr
    | or_expr
    | and_expr
    | eqality_expr
    | relational_expr
    | additive_expr
    | multiplicative_expr
    | alldifferent_constraint
    | sum_constraint
    | count_constraint
    | minimum_constraint
    | maximum_constraint
    | lexicographical_constraint
    | cumulative_constraint
    | value_precede_chain_constraint
    | global_cardinality_constraint
    | diffn_constraint
    | string_equality_constraint
    | redundant_constraint
    ;

forall: FORALL LPAREN generator RPAREN LPAREN expr RPAREN;
exists: EXISTS LPAREN generator RPAREN LPAREN expr RPAREN;
array_acces: ID (LBRACKET (ID | INT) RBRACKET)?;
array_range_acces: ID LBRACKET INT DOTDOT INT RBRACKET;
unary_expr: (NOT | MINUS) expr;
implies_expr: sub_expr (IMPLIES | T_ARROW) sub_expr;
or_expr: sub_expr OR sub_expr;
and_expr: sub_expr AND sub_expr;
eqality_expr: sub_expr (T_EQUALS | T_NOT_EQUALS) sub_expr;
relational_expr: sub_expr (T_LESS_THAN | T_GREATER_THAN | T_LESS_THAN_OR_EQUAL | T_GREATER_THAN_OR_EQUAL) sub_expr;
additive_expr: sub_expr (PLUS | MINUS) sub_expr;
multiplicative_expr: sub_expr (MUL | DIV | MOD) sub_expr;
sub_expr: LPAREN expr RPAREN;
alldifferent_constraint: ALLDIFFERENT LPAREN variable_list RPAREN;
sum_constraint: SUM LPAREN variable_list RPAREN relational_op expr;
count_constraint: COUNT LPAREN variable_list COMMA expr RPAREN relational_op expr;
minimum_constraint: MINIMUM LPAREN variable_list RPAREN relational_op expr;
maximum_constraint: MAXIMUM LPAREN variable_list RPAREN relational_op expr;
lexicographical_constraint: LEXICOGRAPHICAL LPAREN variable_list COMMA variable_list RPAREN relational_op;
cumulative_constraint: CUMULATIVE LPAREN task_list RPAREN relational_op expr;
value_precede_chain_constraint: VALUEPRECEDECHAIN LPAREN INT_list COMMA variable_list RPAREN;
global_cardinality_constraint: GLOBALCARDINALITY LPAREN variable_list COMMA map_of_counts RPAREN;
diffn_constraint: DIFFN LPAREN rect_list RPAREN;
string_equality_constraint: STREQ LPAREN STRING COMMA STRING RPAREN;
redundant_constraint: REDUNDANT LPAREN expr RPAREN;


generator: ID IN INT DOTDOT INT (COMMA ID IN INT DOTDOT INT)*;
variable_list: expr (COMMA expr)*;
INT_list: INT (COMMA INT)*;
task_list: task (COMMA task)*;
task: LPAREN expr COMMA expr COMMA expr RPAREN;
rect_list: rect (COMMA rect)*;
rect: LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN;
map_of_counts: LBRACE key_value (COMMA key_value)* RBRACE;
key_value: INT COLON INT;
relational_op: T_EQUALS | T_LESS_THAN | T_GREATER_THAN | T_LESS_THAN_OR_EQUAL | T_GREATER_THAN_OR_EQUAL;

// Definicje Tokenów
CONSTRAINT: 'constraint ';
SEMI: ';';
NL: '\n';

// Operatory Logiczne
OR: 'or';
AND: 'and';
NOT: 'not';
IMPLIES: 'implies';
T_ARROW: '=>';

// Operatory Relacyjne
T_EQUALS: '==';
T_NOT_EQUALS: '!=';
T_LESS_THAN: '<';
T_GREATER_THAN: '>';
T_LESS_THAN_OR_EQUAL: '<=';
T_GREATER_THAN_OR_EQUAL: '>=';

// Operatory Arytmetyczne
PLUS: '+';
MINUS: '-';
MUL: '*';
DIV: '/';
MOD: '%';

// Nazwy globalnych ograniczeń
ALLDIFFERENT: 'alldifferent';
SUM: 'sum';
COUNT: 'count';
MINIMUM: 'minimum';
MAXIMUM: 'maximum';
LEXICOGRAPHICAL: 'lexicographical';
CUMULATIVE: 'cumulative';
VALUEPRECEDECHAIN: 'valueprecedechain';
GLOBALCARDINALITY: 'globalcardinality';
DIFFN: 'diffn';
STREQ: 'streq';
REDUNDANT: 'redundant';

// Inne
LPAREN: '(';
RPAREN: ')';
LBRACKET: '[';
RBRACKET: ']';
LBRACE: '{';
RBRACE: '}';
COMMA: ',';
COLON: ':';
FORALL: 'forall';
EXISTS: 'exists'; // todo: add all other constraints
IN: 'in';
DOTDOT: '..';

// Typy podstawowe
ID: [a-zA-Z_][a-zA-Z0-9_]*;
INT: [0-9]+;
STRING: '"' ( ~["\r\n] )* '"';

WS: [ \t\r]+ -> skip;