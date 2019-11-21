:- use_module(library(clpfd)).

wolkenkratzer(Board, (Upper, Left, Lower, Right), Max) :-
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
    maplist(skyscrapers, Rows, Left),
    maplist(skyscrapers, ReversedRows, Right),
    maplist(skyscrapers, ReversedColumns, Lower),
    maplist(skyscrapers, Columns, Upper),
    maplist(label, Rows).

diagonal(Rows, Diag) :- length(Rows,N), range(1,N,Indices), maplist(nth1, Indices, Rows, Diag).

range(Low, High, [Low]) :- High =< Low, !.
range(Low, High, [Low|Range]) :- High > Low, Next is Low + 1, range(Next, High, Range), !.

skyscrapers(_, 0) :- !.
skyscrapers([0|Rest], Constraint) :- skyscrapers(Rest, 0, Constraint, 0).
skyscrapers([First|Rest], Constraint) :-
    First #> 0,
    skyscrapers(Rest, First, Constraint, 1).

skyscrapers([], _, Constraint, Constraint).
skyscrapers([First|Rest], Max, Constraint, Num) :-
    (First #> Max,
     Seen is Num + 1,
     skyscrapers(Rest, First, Constraint, Seen),
     !);
    (First #=< Max,
     skyscrapers(Rest, Max, Constraint, Num)).
