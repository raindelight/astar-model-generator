lexer grammar NewMinizincLexer;

VAR: 'var';
PAR: 'par';
INT_TYPE: 'int';
FLOAT_TYPE: 'float';
BOOL_TYPE: 'bool';
RECORD: 'record';
LIST: 'list';
TUPLE: 'tuple';
ARRAY: 'array';
ANN: 'ann';
FUNCTION: 'function';
TEST: 'test';
PREDICATE: 'predicate';
ANNOTATION: 'annotation';
OUTPUT: 'output';
SATISFY: 'satisfy';
MINIMIZE: 'minimize';
MAXIMIZE: 'maximize';
SOLVE: 'solve';
CONSTRAINT: 'constraint';
DOUBLE_PLUS: '++';
INCLUDE: 'include';
ANON_ENUM_T: 'anon_enum';
ENUM_T: 'enum';
ANY: 'any';
TYPE: 'type';
OPT: 'opt';
SET: 'set';
OF: 'of';
STRING_TYPE: 'string';
WHERE: 'where';
FALSE: 'false';
TRUE: 'true';
MOD: 'mod';
DIV_WORD: 'div';
NOT: 'not';
XOR: 'xor';
IN: 'in';
SUBSET: 'subset';
SUPERSET: 'superset';
UNION: 'union';
DIFF: 'diff';
SYMDIFF: 'symdiff';
INTERSECT: 'intersect';
DEFAULT_T: 'default';
IF: 'if';
THEN: 'then';
ELSEIF: 'elseif';
ELSE: 'else';
ENDIF: 'endif';
LET: 'let';

COLONCOLON: '::';
PIPE: '|';
TMUL: '~*';
TDIV: '~/';
TDIV_WORD: '~div';
TMINUS: '~-';
TPLUS: '~+';
POWER: '^';
AREQ: '<->';
IMPL: '->';
REIMPL: '<-';
AND_LOGIC: '\\/';
OR_LOGIC: '/\\';
EQ: '==';
NE: '!=';
GT: '>';
GE: '>=';
LT: '<';
LE: '<=';
TEQ: '~=';
TNEQ: '~!=';
LBAR: '[|';
RBAR: '|]';
PLUS: '+';
MINUS: '-';
DIV: '/';
MUL: '*';
ASSIGN: '=';
COLON: ':';
SEMI: ';';
COMMA: ',';
LPAREN: '(';
RPAREN: ')';
LBRACKET: '[';
RBRACKET: ']';
LBRACE: '{';
RBRACE: '}';
DOTDOT: '..';
DOT: '.';
UNDERSCORE: '_';

FLOAT_LITERAL
    : [0-9]+'.'[0-9]+
    | [0-9]+'.'[0-9]+[Ee][-+]?[0-9]+
    | [0-9]+[Ee][-+]?[0-9]+
    | '0'[xX]([0-9a-fA-F]*'.'[0-9a-fA-F]+ | [0-9a-fA-F]+'.')([pP][+-]?[0-9]+)
    | ([0][xX][0-9a-fA-F]+[pP][+-]?[0-9]+)
    ;

INT_LITERAL
    : [0-9]+
    | '0x'[0-9A-Fa-f]+
    | '0o'[0-7]+
    ;

IDENT: '_'? [a-zA-Z] [a-zA-Z0-9_]*;
QUOTED_IDENT: '`' ( ~[`\r\n\\] | ESCAPE_SEQUENCE )+ '`';
SINGLE_QUOTED_IDENT
    : '\'' (~['\r\n\\] | ESCAPE_SEQUENCE)+ '\''
    ;

STRING_START: '"' -> pushMode(StringMode);

LINE_COMMENT: '%' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
WS: [ \t\r\n]+ -> channel(HIDDEN);

fragment STRING_CHAR: ~["\\\n];
fragment ESCAPE_SEQUENCE: '\\' ('n' | 't' | '"' | '\\' | OCTAL_ESCAPE | HEX_ESCAPE);
fragment OCTAL_ESCAPE: [0-7] [0-7]? [0-7]?;
fragment HEX_ESCAPE: 'x' HEX_DIGIT HEX_DIGIT;
fragment HEX_DIGIT: [0-9a-fA-F];

mode StringMode;
    STRING_END: '"' -> popMode;
    INTERP_START: '(' -> pushMode(DEFAULT_MODE);
    STRING_TEXT: ~[("]+;