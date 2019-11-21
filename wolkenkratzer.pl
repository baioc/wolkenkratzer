:- use_module(library(clpfd)).

wolkenkratzer(Board, (Upper, Left, Lower, Right), Max, DiagCheck) :-
    length(Board, N),
    Min is 1 - (N - Max),
    flatten(Board, Cells),
    Cells ins Min..Max,
    Rows = Board,
    transpose(Rows, Columns),
    maplist(all_distinct, Rows),
    maplist(all_distinct, Columns),
    maplist(reverse, Rows, ReversedRows),
    maplist(reverse, Columns, ReversedColumns),
    diagonal_(DiagCheck, Rows, ReversedRows),
    maplist(skyscrapers, Rows, Left),
    maplist(skyscrapers, ReversedRows, Right),
    maplist(skyscrapers, Columns, Upper),
    maplist(skyscrapers, ReversedColumns, Lower),
    maplist(label, Board).

skyscrapers(_, 0).
skyscrapers([0|Rest], Constraint) :- skyscrapers(Rest, 0, Constraint, 0).
skyscrapers([First|Rest], Constraint) :-
    First #> 0, skyscrapers(Rest, First, Constraint, 1).

skyscrapers([], _, Constraint, Constraint).
skyscrapers([First|Rest], Max, Constraint, Seen) :-
    (First #> Max, Num #= Seen + 1, skyscrapers(Rest,First,Constraint,Num));
    (First #=< Max, skyscrapers(Rest,Max,Constraint,Seen)).

diagonal_(false, _, _).
diagonal_(true, Rows, ReversedRows):-
    diagonal(Rows, MainDiag), all_distinct(MainDiag),
    diagonal(ReversedRows, SecondDiag), all_distinct(SecondDiag).

diagonal(Rows, Diag) :-
    length(Rows, N), range(0, N, Indices), maplist(nth0, Indices, Rows, Diag).

range(Low, High, []) :- High #=< Low.
range(Low, High, [Low|Range]) :-
    High #> Low, Next #= Low + 1, range(Next, High, Range).
