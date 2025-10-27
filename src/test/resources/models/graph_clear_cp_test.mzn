% Adapted from https://github.com/Kurorororo/didp-models/blob/main/graph-clear/graph_clear_cp.py

include "globals.mzn";

int: n;

enum NODE = Node(1..n);

array [NODE] of int: node_weights;
array [NODE, NODE] of int: edge_weights;

int: m = count (i, j in NODE) (edge_weights[i, j] > 0);
enum EDGE = Edge(1..m);
any: edges = array1d(EDGE, [(i, j) | i, j in NODE where edge_weights[i, j] > 0]);

int: z_lb = 1;
int: c_block = sum(edge_weights);
int: z_ub = max(node_weights) + c_block;
int: c_sweep = max(i in NODE) (node_weights[i] + sum(j in NODE) (edge_weights[i, j] + edge_weights[j, i]));

array [NODE] of var NODE: var_t;
array [EDGE] of var NODE: var_l;
array [EDGE] of var NODE: var_u;
array [NODE] of var 1..c_sweep: var_s;
array [NODE] of var 0..c_block: var_b;
array [EDGE, NODE] of var bool: var_i;
var z_lb..z_ub: var_z;

var int: objective = var_z;
solve :: int_search(var_t, smallest, indomain_min) minimize objective;

% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*
