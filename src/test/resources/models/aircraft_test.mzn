% Author: Allen Zhong 2022
% Aircraft Disassembly Scheduling
% Constraint Programming model
% The model is inspired by the following models:
%  - the CP Optimizer model for Aircraft Disassembly Scheduling from https://github.com/cftmthomas/AircraftDisassemblyScheduling
%  - the MiniZinc model for Multi-Skill Project Scheduling Problem from https://github.com/youngkd/MSPSP-InstLib

include "cumulative.mzn";
include "disjunctive.mzn";
% include "redefinitions.mzn";

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% INSTANCE INITIALISATION
int: nActs;
int: nResources;
int: nSkills;
int: nPrecs;
int: nUnavailable;
int: nUnrels; % number of unrelated activites wrt to the precedence graph
int: maxt;  % maximum makespan

set of int: ACT = 1..nActs;
set of int: RESOURCE = 1..nResources;
set of int: SKILL = 1..nSkills;
set of int: PREC = 1..nPrecs;
set of int: UNREL = 1..nUnrels;
set of int: TIME = 0..maxt;
set of int: UNAVAILABLE = 1..nUnavailable;

array[ACT] of set of RESOURCE: USEFUL_RES; % useful resources for each act
array[RESOURCE] of set of ACT: POTENTIAL_ACT; % potential activities for each res

array[ACT] of int: dur; % duration
array[ACT,SKILL] of int: sreq; % task's skill requirement
array[RESOURCE,SKILL] of bool: mastery; % resource's skill mastery
array[RESOURCE] of int: resource_cost; % cost of resource per period
array[PREC] of ACT: pred; % predecessor activity
array[PREC] of ACT: succ; % successor activity
array[UNREL] of ACT: unpred; % unrelated predecessor activity
array[UNREL] of ACT: unsucc; % unrelated successor activity
array[SKILL] of 0..nResources: scap = [sum(r in RESOURCE)(bool2int(mastery[r,s])) | s in SKILL]; % capacity of each skill
array[UNAVAILABLE] of RESOURCE: unavailable_resource;
array[UNAVAILABLE] of TIME: unavailable_start;
array[UNAVAILABLE] of TIME: unavailable_end;

% producer/consumer constraint
array[ACT] of int: mass; % mass required by different activities
set of int: M; % set of mass required to be balanced
array[M, 1..2] of set of LOC: comp_prod; % the producer and consumer location of mass
array[M] of int: maxDiff; % maximum difference of mass

% amount of mass removed from the balance
array[M, ACT] of int: consumption = array2d(M, ACT, [ if loc[j] in comp_prod[i, 1] then mass[j] else if loc[j] in comp_prod[i, 2] then -mass[j] else 0 endif endif | i in M, j in ACT]);
array [M] of set of int: resAct = [ {i | i in ACT where consumption[m, i] != 0} | m in M];  % set of "non-zero" activities for each mass balanced

% Location constraint
int: nLocations;
set of int: LOC = 1..nLocations;
array[ACT] of LOC: loc; % location of activities
array[LOC] of int: loc_cap; % capacity of locations
array[ACT] of int: occupancy; % occupancy requirement

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% DECISION VARIABLES
array[ACT] of var TIME: start; % start time
array[ACT,RESOURCE] of var bool: assign; % assignment of resources to activities
array[ACT,RESOURCE,SKILL] of var bool: contrib; % skill contribution assignment
array[UNREL] of var bool: overlap; % overlapping of unrelated activities

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% CONSTRAINTS


% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*


% minimize the makespan
var int: objective = 100000 * max(start) + sum(a in ACT, r in RESOURCE)(resource_cost[r] * dur[a] * assign[a, r]);
solve :: seq_search([
  int_search(start, input_order, indomain_min),
  bool_search(array1d(assign) ++ array1d(contrib) ++ overlap, input_order, indomain_min)
]) minimize objective;
