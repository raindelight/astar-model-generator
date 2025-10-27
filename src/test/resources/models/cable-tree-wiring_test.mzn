%%%%%%%%%%%%%%%%%%%%%%%%%
% Cable Tree Wiring
%%%%%%%%%%%%%%%%%%%%%%%%%

include "globals.mzn";
int: k;
set of int: Positions = 1..k;
set of int: Cavities = Positions;
int: b;
set of int: ChamberPairs = 1..2*b;
set of int: CableStarts = 1..b;

array[int,int] of Cavities: AtomicConstraints;
array[int,int] of Cavities: DisjunctiveConstraints;
array[int] of Cavities: DirectSuccessors;
array[int,int] of Cavities: SoftAtomicConstraints;

array[Cavities] of var Positions: pfc;


% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*

var int: S = if b=0 then 0 else sum(i in CableStarts) (abs(pfc[i]-pfc[i+b]) > 1) endif;
var int: M = if b=0 then 0 else
	(max(i in ChamberPairs)
		(sum(j in ChamberPairs where j<=b) (pfc[j] < pfc[i] /\ pfc[i] < pfc[j+b])
		 + sum(j in ChamberPairs where j>b) (pfc[j] < pfc[i] /\ pfc[i] < pfc[j-b])))
		 endif;

var int: L = if b=0 then 0 else max(i in CableStarts) (abs(pfc[i]-pfc[i+b])-1) endif;

var int: N = sum(i in index_set_1of2(SoftAtomicConstraints)) (pfc[SoftAtomicConstraints[i,1]] > pfc[SoftAtomicConstraints[i,2]]);

var int: objective = S*pow(k,3)+M*pow(k,2)+L*pow(k,1)+N;

% Redundant dual model via inverse constraint

array[Positions] of var Cavities: cfp;

solve ::int_search(pfc, first_fail, indomain_split) minimize objective;



% *--------------------------------------------------------------------------*
%                   Output was removed for testing
% *--------------------------------------------------------------------------*
