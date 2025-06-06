:- module(safe_cracker, [
    common_constraints/1,
    divides_by/2,
    is_odd/1,
    is_prime/1,
    is_square/1,
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


check_divisors(N, D) :-
    D > floor(sqrt(N)).
check_divisors(N, D) :-
    D =< floor(sqrt(N)),
    \+ divides_by(N, D),
    check_divisors(N, D+1).


is_prime(1) :- !, false.
is_prime(2) :- !.
is_prime(N) :-
    N > 2,
    check_divisors(N, 2).


is_square(N) :-
    N #= _^2.


is_odd(N) :-
    N mod 2 #= 1.


divides_by(X, Y) :-
    X mod Y =:= 0.


occurrenceof([], _, 0). % empty list, count of anything is 0. Base case.

% The first item in the list is the same as what you want to count so
% add 1 to the recursive count.
occurrenceof([H|T], H, NewCount) :-
    occurrenceof(T, H, OldCount),
    NewCount is OldCount + 1.

% The first item in the list is different so keep old count
occurrenceof([H|T], H2, Count) :-
    dif(H, H2),
    occurrenceof(T, H2, Count).


solution_count(Predicate, Count) :-
    Goal =.. [Predicate, A, B, C, D],
    findall([A, B, C, D], Goal, Solutions),
    length(Solutions, Count).
