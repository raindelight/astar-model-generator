% Plik testowy modelu MiniZinc

% --- Parametry (dane wejściowe) ---
int: n_items = 10;
array[1..n_items] of int: profits;
array[1..2] of set of int: item_sets = [{1,2}, {3,4}];
par float: capacity_ratio = 0.5;

int: N_VALUES = 5;
int: N_TASKS = 3;
int: capacity = 5;

% --- Zmienne decyzyjne ---
var 0..1: x;
var bool: is_selected;
array[1..N_VALUES] of var int: values;
array[1..n_items] of var bool: taken;

% -- Zmienne dla Cumulative --
array[1..N_TASKS] of var 0..10: start_times;
array[1..N_TASKS] of var 1..5: durations;
array[1..N_TASKS] of var 1..5: demands;

% -- Zmienne dla Lexicographical --
array[1..3] of var int: lex_a;
array[1..3] of var int: lex_b;

% -- Zmienne dla ValuePrecedeChain --
array[1..5] of var int: precede_chain;

% -- Zmienne dla GlobalCardinality --
array[1..5] of var int: gcc_vars;

% -- Zmienne dla Diffn --
var 0..10: rect1_x; var 0..10: rect1_y; var 1..5: rect1_w; var 1..5: rect1_h;
var 0..10: rect2_x; var 0..10: rect2_y; var 1..5: rect2_w; var 1..5: rect2_h;

solve satisfy;