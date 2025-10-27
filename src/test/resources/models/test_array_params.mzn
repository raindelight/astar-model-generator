% Model z tablicą i zbiorami
int: airlines;
set of int: AIRLINE = 1..airlines;
set of int: FLIGHT;
array[AIRLINE] of set of FLIGHT: FA;