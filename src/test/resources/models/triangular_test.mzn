%------------------------------------------------------------------------------%
% Placing 'Hearts' on Corners in Equilateral Triangular Grids
%
% The problem is to find, for an equilateral triangular grid of
% side N, the maximum number of grid nodes that can have a 'heart' placed
% on them without any placed heart lying on the corners of an equilateral
% triangle of any size or orientation. 
%
% (Taken from Daily Telegraph and Sunday Times)
%------------------------------------------------------------------------------%
% Parameters

int: n;                 % Number of sides of the grid
set of int: N = 1..n;

%------------------------------------------------------------------------------%
% Variables

array [N, N] of var 0..1: heart;    % Grid of equilateral triangulars

var 1..n*n: objective;              % Number of 'hearts' in the grid



% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*


%------------------------------------------------------------------------------%
% Search and Objective

solve 
    :: search
    maximize objective;

ann: search = 
    int_search(
        [heart[i, j] | i in N, j in 1..i], input_order, indomain_max, complete
    );

%------------------------------------------------------------------------------%
% Output

% *--------------------------------------------------------------------------*
%                   Output was removed for testing
% *--------------------------------------------------------------------------*

%------------------------------------------------------------------------------%


