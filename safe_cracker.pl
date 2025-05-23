:- module(safe_cracker, [
    common_constraints/1,
    divides_by/2,
    is_prime/2,
    occurrenceof/3,
    xor/2
]).
:- use_module(library(clpfd)).
:- reexport(library(clpfd)).


common_constraints(Vs) :-
    Vs ins 1..9,
    all_distinct(Vs).


xor(X, Y) :-
    X #/\ #\Y #\/ #\X #/\ Y.


is_prime(N, 1) :- N in {2, 3, 5, 7}.
is_prime(N, 0) :- N in {1, 4, 6, 8, 9}.


divides_by(X, Y) :-
    X mod Y =:= 0.


occurrenceof([], _, 0). %empty list, count of anything is 0. Base case.

% The first item in the list is the same as what you want to count so
% add 1 to the recursive count.
occurrenceof([H|T], H, NewCount) :-
    occurrenceof(T, H, OldCount),
    NewCount is OldCount + 1.

% The first item in the list is different so keep old count
occurrenceof([H|T], H2, Count) :-
    dif(H, H2),
    occurrenceof(T, H2, Count).


