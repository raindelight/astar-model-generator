% MIT License
%
% Copyright (c) 2021 Johan Ludde Wessén
%
% Permission is hereby granted, free of charge, to any person obtaining
% a copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included
% in all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


%=============================================================================%
% YuMiScheduler
%
% Johan Ludde Wessén
% Latest Update 2021-06-23
%============================================================================%

% Changes by MiniZinc Organizers:
% - Ensure max() calls with empty sets do not lead to unsatisfiability

include "globals.mzn";
% include "gecode.mzn";

% model configuration flags
bool: implied_value_precede = true;
bool: implied_cumulative = false;
bool: implied_diffn = false;

int: no_agents = 2; % number of arms
int: no_locations = card(index_set_1of2(left_arm_travel_times) ); % number of locations
int: no_actual_tasks = card(index_set_2of2(task_durations)); % number of actual tasks, i.e. excluding dummy start & end tasks

int: no_tot_tasks = no_actual_tasks+2*no_agents; % all tasks, including dummy start & end tasks

int: min_duration = 1;
int: max_duration = max([ task_durations[r,n] | r in AGENTS, n in ACTUAL_TASKS]);
int: max_travel_time = max([ left_arm_travel_times[i,j] | i,j in LOCATIONS] ++ [ right_arm_travel_times[i,j] | i,j in LOCATIONS] );


int: time_budget_left = sum (i in ACTUAL_TASKS) (
  if -1 < max([left_arm_travel_times[l,l] | l in location_domain(i)]) then
    task_durations[1,i] + max([ left_arm_travel_times[l1,l2] | l1 in location_domain(i), l2 in LOCATIONS ])
  else
    0 % if we can't do the task, we shouldn't count it
  endif
);

int: time_budget_right = sum (i in ACTUAL_TASKS) (
  if -1 < max([right_arm_travel_times[l,l] | l in location_domain(i)]) then
    task_durations[2,i] + max([ right_arm_travel_times[l1,l2] | l1 in location_domain(i), l2 in LOCATIONS ])
  else
    0  % if we can't do the task, we shouldn't count it
  endif
);
int: time_budget = max([time_budget_left,time_budget_right]);


% Calculate the absolute minimum time an arm needs to work + travel
int: min_time_budget_left = sum (i in ACTUAL_TASKS) (
  if -1 < min([left_arm_travel_times[l,l] | l in location_domain(i)]) then
    task_durations[1,i] + min([ left_arm_travel_times[l1,l2] | l1 in location_domain(i), l2 in LOCATIONS ])
  else
    0 % if we can't do the task, we shouldn't count it
  endif
);

int: min_time_budget_right = sum (i in ACTUAL_TASKS) (
  if -1 < min([right_arm_travel_times[l,l] | l in location_domain(i)]) then
    task_durations[2,i] + min([ right_arm_travel_times[l1,l2] | l1 in location_domain(i), l2 in LOCATIONS ])
  else
    0  % if we can't do the task, we shouldn't count it
  endif
);
int: min_time_budget = min([min_time_budget_left,min_time_budget_right]);

% Calculate the time if one arm recieves all tasks it can perform, and manages to get minimal travel time
int: minimax_time_budget_left = sum (i in ACTUAL_TASKS) (
  if -1 < max([left_arm_travel_times[l,l] | l in location_domain(i)]) then
    task_durations[1,i] + min([ left_arm_travel_times[l1,l2] | l1 in location_domain(i), l2 in LOCATIONS ])
  else
    0 % if we can't do the task, we shouldn't count it
  endif
);

int: minimax_time_budget_right = sum (i in ACTUAL_TASKS) (
  if -1 < max([right_arm_travel_times[l,l] | l in location_domain(i)]) then
    task_durations[2,i] + min([ right_arm_travel_times[l1,l2] | l1 in location_domain(i), l2 in LOCATIONS ])
  else
    0  % if we can't do the task, we shouldn't count it
  endif
);

int: minimax_time_budget = min([minimax_time_budget_left,minimax_time_budget_right]);
int: min_traveltime = 1;
int: min_waittime = 1;
set of int: TOTTIME = 0..time_budget;
set of int: WAITTIME = min_waittime..time_budget;


set of int: DURTIME = min_duration..max_duration; %smaller domain, for task durations. needs to include 0, due to dummy tasks
set of int: TRAVELTIME = min_traveltime..max_travel_time; % smaller domain, for travel times, needs to include 0, due to dummy tasks

% The last tasks represent the start and end task for each vehicle (dummies, not depot)
set of int: AGENTS = 1..no_agents;
set of int: TASKS = 1..no_tot_tasks;
set of int: ACTUAL_TASKS = 1..no_actual_tasks;

set of int: LOCATIONS = 1..no_locations;


% Depot tasks are not actual tasks, but rather a modelling convenience, that separates our
% successors into 2 sequences
set of int: DEPOT_TASKS = no_actual_tasks+1..no_tot_tasks;
set of int: START_DEPOT_TASKS = no_actual_tasks+1..no_actual_tasks+no_agents;
set of int: END_DEPOT_TASKS = no_actual_tasks+no_agents+1..no_tot_tasks;

array[int] of int: DEPOT_TASK_LIST = [no_actual_tasks + a | a in 1..2*no_agents];
array[int] of int: START_DEPOT_TASK_LIST = [no_actual_tasks + a | a in AGENTS];
array[int] of int: END_DEPOT_TASK_LIST = [no_actual_tasks + no_agents + a | a in AGENTS];

%To be defined in datafile:
% TODO: generalize
set of int: TRAY_TASKS;
set of int: CAMERA_TASKS;
set of int: FIXTURE_TASKS = all_tasks(fixture_task_orders);
set of int: GRIPPER_TASKS = all_tasks(gripper_pick_tasks_orders);
set of int: SUCTION_TASKS = all_tasks(suction_pick_tasks_orders);
set of int: OUTPUT_TASKS;
set of int: empty_gripper_tasks; % these are tasks for which the application requires empty gripper (e.g. "peeling of tape")


% We assume these sets have no overlap, and form the full set of locations
set of int: TRAY_LOCATIONS;
set of int: CAMERA_LOCATIONS;
set of int: FIXTURE_LOCATIONS;
set of int: AIRGUN_LOCATIONS;
set of int: OUTPUT_LOCATIONS;

% Data
array[AGENTS, int] of int: task_durations;
array[int,int] of int: left_arm_travel_times;
array[int,int] of int: right_arm_travel_times;
array[int] of int: location_order;

% =================================================%
% Variables
% =================================================%
% These variables each belongs to a task.
% =================================================%
%
% Since we model multiple (2) agents using 1 hamiltonian task circuit
% we add dummy tasks as "breaks" between each agent's task sequence.
% The allover sequence starts with the start task of agent 1, the tasks of agent 1,
% and finishes with the end task of agent1. It then continues with the start task
% of agent 2, the tasks of agent 2, and finishes with the end task of agent2.
% The end task of agent 2 then finalizes the circuit by "pointing to" the
% start task of agent 1.
%

array[TASKS] of var AGENTS: agent :: add_to_output; % Which agent performs the task

% Route encodings - each of these encode the same information, but in different ways
array[TASKS] of var TASKS: successor; % Task sequence encoding where successor[i] = j means task j follows task i
array[TASKS] of var TASKS: task :: add_to_output; % Task sequence encoding where task[i] = j means task j is i:th to be performed

% Time variables:
array[TASKS] of var TOTTIME: arrival_time :: add_to_output; % when agent arrives at task
array[TASKS] of var TOTTIME: start_time; % when agent start working on task
array[TASKS] of var TOTTIME: end_time; % when agent finish working on task
array[TASKS] of var TOTTIME: next_arrival_time; % when agent arrives at next task

array[TASKS] of var WAITTIME: waiting_time; % Time between arrival_time and start_time (needed(?) to ensure collision detection at all times)
array[TASKS] of var DURTIME: duration; % duration of task
array[TASKS] of var TRAVELTIME: travel_time; % travel time of going from task i to successor[i]


% Locations - locations are variable
array[TASKS] of var LOCATIONS: location :: add_to_output;
array[TASKS] of var LOCATIONS: next_location; % Used to induce traveltime between tasks



% =================================================%
% Variables not attributet to one task
% =================================================%

% META VARIABLES - used for search & increasing propagation
array[1..1,AGENTS] of var TASKS: agent_counts;
var TASKS: agent_count = 2*length(agent) - sum(agent); % # tasks assigned to left arm


% =================================================%

% Routing Model Constraints:
%
% Each arm task sequence starts with a (dummy) start task, and a (dummy) end task
% Since it is all modelled as one Hamiltonian Circuit, we need to connect arm sequences to each other
% Thus,
% - After end task of arm n comes start task of arm n+1
% - After end task of arm N comes start task of arm 0
% We use 4 equivalent Encodings:
% (A) successor
% (B) task

% =================================================%
% Constraint related to the dummy tasks:
% [..., START_DEPOT_TASK_LIST[1], START_DEPOT_TASK_LIST[2], END_DEPOT_TASK_LIST[1], END_DEPOT_TASK_LIST[2]]
% =================================================%

% -----------------------------------------------------
% Constraints on dummy tasks' sequence encodings

% **********************
%Constraint 1, row 3:
% **********************
% Encoding A:

% Encoding B: (only states start & end of full sequence)START_DEPOT_TASK_LIST[1]] = END_DEPOT_TASK_LIST[2];


% Making sure we get the full cost of the schedule:
% Make an artificial "depot"
% Return the arm to 1:st position at the end of a schedule
% The last task in the sequence should reflect that it returns to the first task of the cycle
%  Thus, we make sure that the end task get the same location as the successor of the start task of the arm
%-------------------------------------------------------
% Constraint 1, row 5-6:

%-------------------------------------------------------
% ----- Dummy Tasks Time Constraints ------------
%-------------------------------------------------------


% Overlapping cycles: ------------------------------------------

% At least one agent starts at time 0


%===========================================
% END DUMMY TASK CONSTRAINTS
%===========================================


%===========================================
% Route Model Constraints
%===========================================

% ---------------------------------------------------------------------
% Hamiltonian Circuit
%--------------------------------

% ----------------------------------------------------------------------
% Connect Successor Encoding (A) with "task" Encoding (B)
% ----------------------------------------------------------------------

% Redundant


%-----------------------------------------------------------------------------------
% Connecting Successor Encoding (A) with Route Assignment:
% Posting that successors & predeccessors has same agent (agent)
%------------------------------------------------------------------------------------


% ==============================================
% Locations Constraints:
% ==============================================
int: no_fixture_locations = card(FIXTURE_LOCATIONS);

% Only allowing locations of correct type
predicate remove_invalids(set of int: tasks, set of int: locs) =
  forall(t in tasks) (location[t] in locs) :: domain_change_constraint;

% Pre Constraint 7


% =======================================================
% Suction Tool Task Arm Orderings
% =======================================================
int: no_suction_cups;

array[int,int] of int: suction_pick_tasks_orders;
int: no_suction_picks = card(index_set_1of2(suction_pick_tasks_orders));
int: suction_order_lengths = card(index_set_2of2(suction_pick_tasks_orders));


% -------------------------------------------------------
%  Ordering constraints on routing model
% -------------------------------------------------------






% ========================================================
% Gripper Tool Tasks Arm Orderings
% ========================================================
array[int,int] of int: gripper_pick_tasks_orders;
int: no_gripper_picks = card(index_set_1of2(gripper_pick_tasks_orders));
int: gripper_order_lengths = card(index_set_2of2(gripper_pick_tasks_orders));

% -------------------------------------------------------
% -------------------------------------------------------

% All pick-n-place belongs to same arm
% We know the ordering value will be increasing



% =====================================================================
% Fixture orderings - regex for agent model + interval constraints
% =====================================================================
array[int,int] of int: fixture_task_orders;
int: no_fixtures = card(index_set_1of2(fixture_task_orders));
int: fixture_order_lengths = card(index_set_2of2(fixture_task_orders));

% -------------------------------------------------------
% ------------------------------------------------------


% -----------------------------------------------------------
% END FIXTURE ORDERS
% -----------------------------------------------------------


% =============================================================
% Constraint Specific to the reality of assembly layout
% =============================================================

% All component trays need to be at different locations

% =============================================================
% END Assembly Layout Constraint
% =============================================================

% =============================================================
% Collision Coordination
% =============================================================

% Constraint 8, and equivalents - in separate file(s)

array[1..2] of int: FixtureWorkObstruction;

% Making sure fixture is emptied before next assembly:

% Making sure no overlap of assembly tasks within an agent (might remove some solutions, but a reasonable simplification as to facilitate pick-up-and-delivery constraints hold):

% =============================================================
% END Collision Coordination
% =============================================================

% =============================================================
% START Symmetry Breaking
% =============================================================

% None so far..

% =============================================================
% END Symmetry Breaking
% =============================================================


% =====================================
% Objective - and related constraints
% =====================================

% cycle overlap is the time between the finish time of one arm and the finish time of the other arm
% (it is also the time between the start time of one arm and the start time of the other arm)

var TOTTIME: cycle_overlap ;

set of int: FINISH_TIMES = (minimax_time_budget div 2)..time_budget;
var FINISH_TIMES: period ;
var FINISH_TIMES: makespan;





% Enforce sequencing of fixture tasks
% Enforce sequencing of suction PnP tasks

% Enforce sequencing of gripper PnP tasks

% Gripper capacity constraint /\ empty gripper constraint
%----------------------------------------------------

%----------------------------------------------------------------
% Helper functions for orderings (name suggests functionality)
%----------------------------------------------------------------

% Return number of values in the sequence until we meet a -1 , i.e. index of last value = # of useful numbers in sequences
function int: sequence_length(array[int,int] of int: task_orders, int: sequence_no) =
    if card(index_set_1of2(task_orders)) = 0 then 0 else sequence_length(task_orders, sequence_no, 1) endif;

% Implementation of above, but requires additional index as input
function int: sequence_length(array[int,int] of int: tasks_orders, int: sequence_no, int: index) =
    if tasks_orders[sequence_no, index] < 0 then index-1 else sequence_length(tasks_orders, sequence_no, index+1) endif;

% Return all tasks of sequence
function set of int: all_tasks_of_sequence(array[int,int] of int: task_orders, int: c) = {
    task_orders[c,j] | j in index_set_2of2(task_orders) where task_orders[c,j] > 0 };

% Return all tasks of table
function set of int: all_tasks(array[int,int] of int: task_orders) =
    if card(index_set_1of2(task_orders)) = 0 then {} else all_tasks(task_orders, card(index_set_1of2(task_orders))) endif;

% Implementation of above
function set of int: all_tasks(array[int,int] of int: task_orders, int: c) =
    if c <= 1 then all_tasks_of_sequence(task_orders, c) else all_tasks(task_orders, c-1) union all_tasks_of_sequence(task_orders, c) endif;

% We often need the location domain of tasks, this should provide that
function set of int: location_domain(int: task) =
  if task in TRAY_TASKS then
    TRAY_LOCATIONS
  elseif task in CAMERA_TASKS then
    CAMERA_LOCATIONS
  elseif task in OUTPUT_TASKS then
    OUTPUT_LOCATIONS
  else
    LOCATIONS % The task does not belong to the categories.. should be start or end task
  endif;


solve :: seq_search([
		     int_search(agent, smallest, indomain_min, complete),
		     int_search(location, smallest, indomain_min, complete),
		     int_search(task, input_order, indomain_min, complete),
		     int_search(arrival_time, smallest, indomain_split, complete) % removed comma TODO
                    ])
  minimize period;

%=============================================================================%
% YuMiScheduler Collision Model
%
% Johan Ludde Wessén
% Latest Update 2021-02-21
%============================================================================%
bool: using_cumulative_per_zone = true;
bool: using_diffn_per_zone = false;
bool: using_unified_diffn = false;

int: max_lwait = max( [ max(e) default 0 | e in wait_zones_left_data]  );
int: max_rwait = max( [ max(e) default 0 | e in wait_zones_right_data] );
int: max_lwork = max( [ max(e) default 0 | e in work_zones_left_data]  );
int: max_rwork = max( [ max(e) default 0 | e in work_zones_right_data] );


int: max_zones = max([max_lwait, max_rwait, max_lwork, max_rwork]);

set of int: ZONES = 0..max_zones;
set of int: FIXTURE_ZONES = (max_zones - no_fixture_locations + 1)..max_zones; %by design the fixture zones are last
set of int: ZONES_DIFF_FIXTURES = ZONES diff FIXTURE_ZONES;


array[int] of set of int:  wait_zones_left_data;
array[int] of set of int:  wait_zones_right_data;
array[int] of set of int:  work_zones_left_data;
array[int] of set of int:  work_zones_right_data;

array[int,int] of set of int: travel_zones_left_data;
array[int,int] of set of int: travel_zones_right_data;

% We are assigning 1 if any location in the set of location and next location blocks the current location-zone
array [ACTUAL_TASKS, ZONES] of var 0..1: task_blocker;

% Table to assign if a zone is occupied or blocked (for the other arm) by a travel between 2 locations
% This table is generated from the data, simulating 2 armed YuMi assembly cell described in the paper [To be inserted]

% Analogous to work_blocker, but for waiting
array [ACTUAL_TASKS, ZONES] of var 0..1: wait_blocker;

% Table to assign if a zone is occupied while waiting at a location


% Analougous to work_blocker
array [ACTUAL_TASKS, ZONES] of var 0..1: travel_blocker;

% Table to assign if a zone is occupied or blocked by a travel between 2 locations


%------------------
% This is the default (full) formulation, however this does _not_ work well if
% any of the variables in the second argument (dur) has zero in its domain
% Having zero in such domain gives a very poor decomposition when using the default solver (Gecode)

% Note: If any variable of the second argument needs to be zero, there are other, less
% intuitive formulations that are specific to the workspace and robot setup at hand.

% Unified diffn formulation



% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*
% *--------------------------------------------------------------------------*
%                   Output was removed for testing
% *--------------------------------------------------------------------------*
