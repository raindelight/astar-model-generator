% Copyright 2024 Maxime Mahout, François Fages
% MIT License

% variables
int: n; %nb of reactions
int: m; %nb of metabolites
int: k; %nb of reversible reactions

enum Reactions = _(1..n);
enum Metabolites = _(1..m);
enum Reversibles = _(1..k);

array[Reactions] of string: Rs;        % reactions
array[Metabolites] of string: Ms;        % metabolites
array[Metabolites, Reactions] of int: S;         % stoichiometry matrix
array[Reversibles, Reactions] of int: Revs;        % reversibles indicators

int: iub = 50; % integer upper bound

% intermediate variables
% efm values
array[Reactions] of var 0..iub: Vs;  % integer flux
% efm support
array[Reactions] of var bool: Zs :: output_var;  % support


% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*


solve :: bool_search(Zs, first_fail, indomain_min, complete) 
         minimize sum(Zs);


% *--------------------------------------------------------------------------*
%                   Output was removed for testing
% *--------------------------------------------------------------------------*

