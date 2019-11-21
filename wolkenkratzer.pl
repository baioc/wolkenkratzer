:- use_module(library(clpfd)).

wolkenkratzer(Board, (Upper, Left, Lower, Right), Max, IsDiag) :-
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
    diagonal(IsDiag, Rows, ReversedRows),
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

diagonal(false, _, _).
diagonal(true, Rows, ReversedRows):-
    diagonal(Rows, PrimDiag),
    diagonal(ReversedRows, SecondDiag),
    all_distinct(PrimDiag),
    all_distinct(SecondDiag).
diagonal(Rows, Diag) :-
    length(Rows, N), range(0, N, Indices), maplist(nth0, Indices, Rows, Diag).

range(Low, High, []) :- High #=< Low.
range(Low, High, [Low|Range]) :-
    High #> Low, Next #= Low + 1, range(Next, High, Range).

%% https://www.janko.at/Raetsel/Wolkenkratzer/242.a.htm
% ?- Board =  [[_,_,_,_],
%              [_,_,_,_],
%              [_,_,_,_],
%              [_,_,_,_]],
%    Board = [A,B,C,D],
%    wolkenkratzer(Board, ([0,0,4,0], [0,0,0,0], [0,2,0,0], [1,4,0,0]), 4, false).
%
% Board = [[3, 2, 1, 4], [4, 3, 2, 1], [1, 4, 3, 2], [2, 1, 4, 3]],
% A = [3, 2, 1, 4],
% B = [4, 3, 2, 1],
% C = [1, 4, 3, 2],
% D = [2, 1, 4, 3] ;
% false.

%% https://www.janko.at/Raetsel/Wolkenkratzer/383.a.htm
% ?- Board =  [[_,_,_,_],
%              [_,_,_,_],
%              [_,_,_,_],
%              [_,_,_,_]],
%    Board = [A,B,C,D],
%    wolkenkratzer(Board, ([0,0,0,1], [0,2,0,2], [0,0,3,0], [0,0,0,0]), 3, false).
%
% Board = [[1, 2, 0, 3], [0, 1, 3, 2], [3, 0, 2, 1], [2, 3, 1, 0]],
% A = [1, 2, 0, 3],
% B = [0, 1, 3, 2],
% C = [3, 0, 2, 1],
% D = [2, 3, 1, 0] ;
% false.

%% https://www.janko.at/Raetsel/Wolkenkratzer/005.a.htm
% ?- Board =  [[_,_,_,_],
%              [_,_,_,_],
%              [_,_,_,_],
%              [1,_,_,_]],
%    Board = [A,B,C,D],
%    wolkenkratzer(Board, ([0,0,0,0], [1,2,3,0], [0,0,0,0], [0,0,0,0]), 4, true).
% %
% Board = [[4, 1, 2, 3], [3, 4, 1, 2], [2, 3, 4, 1], [1, 2, 3, 4]],
% A = [4, 1, 2, 3],
% B = [3, 2, 4, 1],
% C = [2, 3, 1, 4],
% D = [1, 4, 2, 3] ;
% false.
