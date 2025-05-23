:- module(safe_cracker, [
    common_constraints/1,
    is_prime/2,
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
