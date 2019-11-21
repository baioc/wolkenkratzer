:- use_module(library(clpfd)).

wolkenkratzer(Board, (Upper, Left, Lower, Right), N) :-
    flatten(Board, Cells),
    Cells ins 1..N,
    Rows = Board,
    transpose(Rows, Columns),
    maplist(reverse, Rows, ReversedRows),
    maplist(reverse, Columns, ReversedColumns),
    maplist(skyscrapers, Rows, Left),
    maplist(skyscrapers, ReversedRows, Right),
    maplist(skyscrapers, Columns, Upper),
    maplist(skyscrapers, ReversedColumns, Lower),
    maplist(all_distinct, Rows),
    maplist(all_distinct, Columns),
    maplist(label, Rows).

skyscrapers(_, 0) :- !.
skyscrapers([First|Rest], Constraint) :-
    skyscrapers(Rest, First, Constraint, 1).

skyscrapers([], _, Constraint, Constraint).
skyscrapers([First|Rest], Max, Constraint, Num) :-
    (First #> Max,
     Seen #= Num + 1,
     skyscrapers(Rest, First, Constraint, Seen),
     !);
    (First #=< Max,
     skyscrapers(Rest, Max, Constraint, Num)).
