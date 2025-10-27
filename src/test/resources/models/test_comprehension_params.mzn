% Model z nową składnią array comprehension
set of int: FLIGHT;
set of int: TIME;
array[FLIGHT] of int: xCoor;
array[FLIGHT] of int: opDur;

array[FLIGHT] of set of TIME: ISet =
    [ {k| k in xCoor[f]..(opDur[f] + xCoor[f] - 1)} | f in FLIGHT];