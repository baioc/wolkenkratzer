:- use_module(library(clpfd)). %Constraint Logic Programming over Finite Domains

/* solves an instance of the skyscrapers puzzle defined by given constraints
  (0s are ignored), maximum building height and the option to check diagonals */
wolkenkratzer(Board, (Upper, Left, Bottom, Right), Max, DiagCheck) :-
    % for a NxN board, each list of constraints has N values
    length(Board, N),
    length(Upper, N),
    length(Left, N),
    length(Bottom, N),
    length(Right, N),

    % cells can be filled with an integer value in the range [Min,Max]
    Min is 1 - (N - Max),
    Min >= 0,
    flatten(Board, Cells),
    Cells ins Min..Max,

    % rows dont repeat values and neither do columns
    Rows = Board,
    transpose(Rows, Columns),
    maplist(all_distinct, Rows),
    maplist(all_distinct, Columns),
    maplist(reverse, Rows, ReversedRows),
    maplist(reverse, Columns, ReversedColumns),

    % sometimes, diagonals need to be distinct as well
    diagonal_(DiagCheck, Rows, ReversedRows),

    % apply pruning
    maplist(pruned(Min,Max), Rows, Left),
    maplist(pruned(Min,Max), ReversedRows, Right),
    maplist(pruned(Min,Max), Columns, Upper),
    maplist(pruned(Min,Max), ReversedColumns, Bottom),

    % check if all constraints are met
    maplist(skyscrapers, Rows, Left),
    maplist(skyscrapers, ReversedRows, Right),
    maplist(skyscrapers, Columns, Upper),
    maplist(skyscrapers, ReversedColumns, Bottom),

    % collapse cell domains into single values
    maplist(label, Board).


% counts the number of visible skyscrapers on a given sequence
skyscrapers(_, 0).
skyscrapers([0|Rest], Constraint) :- skyscrapers(Rest, 0, Constraint, 0).
skyscrapers([First|Rest], Constraint) :-
    First #> 0, skyscrapers(Rest, First, Constraint, 1).

/* checks whether the number of visible skyscrapers in given sequence satisfies
   a constraint, starting with some max height and a previous count */
skyscrapers([], _, Constraint, Constraint).
skyscrapers([First|Rest], Max, Constraint, Seen) :-
    (First #> Max, Num #= Seen + 1, skyscrapers(Rest,First,Constraint,Num));
    (First #=< Max, skyscrapers(Rest,Max,Constraint,Seen)).

% auxiliary predicate that checks both wolkenkratzer diagonals only if needed
diagonal_(false, _, _).
diagonal_(true, Rows, ReversedRows):-
    diagonal(Rows, MainDiag), all_distinct(MainDiag),
    diagonal(ReversedRows, SecondDiag), all_distinct(SecondDiag).

% extracts main diagonal from a list of lists
diagonal(Rows, Diag) :-
    length(Rows, N), range(0, N, Indices), maplist(nth0, Indices, Rows, Diag).

% generates the ordered sequence of integers in [Low, High)
range(Low, High, []) :- High #=< Low.
range(Low, High, [Low|Range]) :-
    High #> Low, Next #= Low + 1, range(Next, High, Range).

%% applies pruning laws to further constrain value domains
% zero restrictions are ignored
pruned(_, _, 0).

% when tip is 1 and no parks are allowed, first cell is N
pruned(1, N, [N|_], 1).

% when tip is 1 and parks are allowerd, first cell is either Max or 0
pruned(0, Max, [First|_], 1) :- First #= Max; First #= 0.

% when tip is N, line is [1..N] (no parks)
pruned(1, N, Sequence, N) :- End #= N + 1, range(1, End, Sequence).

% when tip is some K, apply an upper bound to the whole line
pruned(_, N, Line, Tip) :- pruned_(N, Line, Tip).
pruned_(_, _, 0).
pruned_(_, [], _).
pruned_(N, [First|Rest], Tip) :-
    K is Tip - 1, First #=< N - K, pruned_(N, Rest, K).
