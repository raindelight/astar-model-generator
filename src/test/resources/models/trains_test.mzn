% train scheduling
enum STOP;                             % set of stops
STOP: dstop;                           % dummy stop
array[STOP] of int: minimal_wait;      % minimum wait time at station 
array[STOP,STOP] of opt 0..infinity: travel_time; % travel time from each stop to another <> if not possible
array[STOP] of int: platform;          % number of platforms at station
enum STYPE = { ORDINARY, HUB, TERMINUS };
array[STOP] of STYPE: stype;           % type of STOP
array[STOP] of int: skip_cost;         % cost to skip the station (per service)
enum LINE = { SING, DOUB, QUAD, NONE };
array[STOP,STOP] of LINE: line;        % line type between stops
                  
enum ROUTE;                            % a sequence of STOPs
int: max_route_length;                 % longest route in number of stops
set of int: STOPNO = 1..max_route_length;         
array[ROUTE] of STOPNO: rlength;       % length of route
array[ROUTE,STOPNO] of STOP: route;    % route definitions; 
   
enum SERVICE;                          % the set of services to be schedules                         
array[SERVICE] of ROUTE: sroute;       % the route for each service
int: makespan;                         % end of time considered;
set of int: TIME = 0..makespan;
TIME: min_sep;                         % min separation of services on the same line same direction
array[SERVICE] of TIME: service_start; % from when the service can start
array[SERVICE] of TIME: service_end;   % when the service ideally ends

enum ENGINE;                           % set of engines available
array[ENGINE] of STOP: start;          % start location for engines
enum SERVICEX = s(SERVICE) ++ e(ENGINE); 

%%%%%%%%%%% Assertions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*

%%%%%%%%%%% Decisions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

include "globals.mzn";

array[SERVICE,STOPNO] of var TIME: arrive;    % arrive time at each stop
array[SERVICE,STOPNO] of var TIME: depart;    % depart time at each stop
array[SERVICE,STOPNO] of var TIME: wait;      % wait time at each stop
array[SERVICE,STOPNO] of var bool: stopped;   % which stops are actually made                                                            
var 0..infinity: delay_obj;
var 0..infinity: skip_obj; 
array[SERVICE] of var ENGINE: engine;         % which engine is used for each service
array[SERVICE] of var SERVICEX: prev;         % previous service or engine

%%%%%%%%%%% STAGE A - schedule constraints 
include "globals.mzn";


% *--------------------------------------------------------------------------*
%                   Output was removed for testing
% *--------------------------------------------------------------------------*
