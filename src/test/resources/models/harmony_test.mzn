include "globals.mzn";

% MIDI notes
enum Pitch = Pitch(0..127);
% Musical notes
enum Note = {C, 'C#', Db, D, E, 'D#', Eb, F, 'F#', Gb, G, 'G#', Ab, A, 'A#', Bb, B};

%============================= CHANGED SECTION [ Illegal Pitch^-1() ] ================================
% Convert note and octave into MIDI pitch
function Pitch: note(Note: note, int: octave) =
  let {
    array[Note] of int: semitones = [
      C -> 0, 'C#' -> 1, Db -> 1, D -> 2, 'D#' -> 3, Eb -> 3,
      E -> 4, F -> 5, 'F#' -> 6, Gb -> 6, G -> 7, 'G#' -> 8,
      Ab -> 8, A -> 9, 'A#' -> 10, Bb -> 10, B -> 11
    ];
    int: offset = semitones[note];
    int: start  = (octave + 1) * 12
  } in Pitch(start + offset);

enum Chord = { I, ii, iii, IV, V, V7, vi };

array [Chord] of set of int: chord_pitches = [
  I   : {0, 4, 7},
  ii  : {2, 5, 9},
  iii : {4, 7, 11},
  IV  : {5, 9, 12},
  V   : {7, 11, 14},
  V7  : {7, 11, 14, 17},
  vi  : {9, 12, 16}
];

array [Chord] of set of int: chord_offsets =
  [ c : { (v + (note(key,4) mod 12)) mod 12 | v in chord_pitches[c] }
  | c in Chord ];

%============================= CHANGED SECTION END ================================

% Cadences
any: perfect_cadences = [(V, I), (V7, I)];
any: plagal_cadences = [(IV, I)];
any: imperfect_cadences = [(I, V), (ii, V), (IV, V), (vi, V)];
any: interrupted_cadences = [(V, vi), (V7, vi)];
any: all_cadences = perfect_cadences ++ plagal_cadences ++ imperfect_cadences ++ interrupted_cadences;

bool: enforce_cadences;
int: min_perfect;
int: min_plagal;
int: min_imperfect;
int: min_interrupted;

int: max_stationary;

% Voices
enum Voice = { Soprano, Alto, Tenor, Bass };
array [Voice] of set of Pitch: range = [
  Soprano: note(C, 4)..note(G, 5),
  Alto: note(G, 3)..note(C, 5),
  Tenor: note(C, 3)..note(G, 4),
  Bass: note(F, 2)..note(C, 4)
];
set of Pitch: used_range = let {
  any: u = array_union(range);
} in min(u)..max(u);
set of Voice: otherVoices = Voice diff {Soprano};

% Number of chords to harmonise
int: maxTime = length(melody);
enum Time = Time(index_set(melody));

% Home key
Note: key;

array [int] of Pitch: melody;

% constraint trace("\(melody)\n");

% The notes to play
array [Voice, Time] of var Pitch: music;
array [Time] of var Chord: chords;

% *--------------------------------------------------------------------------*
%                    Constraints removed for testing
% *--------------------------------------------------------------------------*


%============================= CHANGED SECTION [ Illegal Pitch^-1() ] ================================
any: note_numbers =
  [ music[v, t] mod 12 | v in Voice, t in Time ];
%============================= CHANGED SECTION END ================================


% The notes played must be the notes of the chosen chord
any: chord_notes = [{ note_numbers[v, t] | v in Voice } | t in Time];


function var int: interval(Voice: v, Voice: u, var Time: t) = abs(music[v, t] - music[u, t]) mod 12;


% Largest jump by each voice
any: max_jump = [v: max (t in Time where t < max(Time)) (abs(music[v, t] - music[v, enum_next(t)])) | v in Voice];

% Range used by each voice
%============================= CHANGED SECTION [ Illegal .. ] ================================
%any: range_used = [v: max(music[v, ..]) - min(music[v, ..]) + 1 | v in Voice];
any: range_used = [ max([music[v, t] | t in Time]) - min([music[v, t] | t in Time]) + 1 | v in Voice ];
%============================= CHANGED SECTION END ================================
% Number of non-root position chords

%============================= CHANGED SECTION [ Illegal output ] ================================
%var int: non_root :: output = count (t in Time) (note_numbers[Bass, t] != min(chord_notes[t]));
var int: non_root :: output_var =
    count(t in Time) (note_numbers[Bass, t] != min(chord_notes[t]));

%============================= CHANGED SECTION END ================================

% Number of chords with a note other than the root doubled
%============================= CHANGED SECTION [ Illegal output ] ================================
%var int: non_doubled_root :: output = count (t in Time) (count (v in Voice) (note_numbers[v, t] == min(chord_notes[t])) == 1);
var int: non_doubled_root :: output_var = count (t in Time) (count (v in Voice) (note_numbers[v, t] == min(chord_notes[t])) == 1);
%============================= CHANGED SECTION END ================================

% End of every 4 chords is a cadence
%============================= CHANGED SECTION [ Illegal output ] ================================
any: cadences :: output_var = [(chords[Time(t)], chords[Time(t + 1)]) | t in 1..maxTime where t mod 4 = 3];


% Objective
%============================= CHANGED SECTION [ Illegal output ] ================================
var int: objective :: output_var = sum(max_jump) + non_root + non_doubled_root;

enum SearchVar = { ChordVar } ++ VoiceVar(Voice);
solve :: int_search(
  [
    if x = ChordVar then
      let { var int: v = chords[t]; } in v
    else
      let { var int: v = music[VoiceVar^-1(x), t]; } in v
    endif
  |
    t in Time,
    x in SearchVar
  ],
  input_order,
  indomain_min
) minimize objective;



% *--------------------------------------------------------------------------*
%                   Output was removed for testing
% *--------------------------------------------------------------------------*
