% Monitor 1id Problem
int: n; % number of nodes
int: r; % number of routes   
int: b; % number of biconnected components with only one articulation point
array[1..r,1..2] of int: routes_ends; % set of routes, contains index of starting and ending nodes of routes
array[1..r] of set of int: routes; % representation of routes, contains the set of nodes crossed by each route
array[1..b] of set of int: bi_comp; % biconnected components with only one articulation point for redundant constraints
array[int] of int: leaf_nodes; % list of independant nodes that need to be monitors

array[1..n] of var bool:x; % monitors
array[1..r] of var bool:y; % measurement paths


% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*

% the goal is to minimize the number of monitors
solve :: bool_search(x ++ y, input_order, indomain_min) minimize(sum(x));

% *--------------------------------------------------------------------------*
%                   Output was removed for testing
% *--------------------------------------------------------------------------*
