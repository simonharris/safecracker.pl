:- module(safe_cracker, [
    common_constraints/1,
    divides_by/2,
    is_odd/1,
    is_even/1,
    is_prime/1,
    is_prime/2,
    is_square/1,
    occurrenceof/3,
    xor/2
]).
:- use_module(library(clpfd)).
:- reexport(library(clpfd)).


common_constraints(Vs) :-
    Vs ins 1..9,
    all_distinct(Vs),
    labeling([], Vs).


xor(X, Y) :-
    X #/\ #\Y #\/ #\X #/\ Y.

% Recursive is_prime/1 implementation....

% check_divisors(N, D) :-
%     D > floor(sqrt(N)).
% check_divisors(N, D) :-
%     D =< floor(sqrt(N)),
%     \+ divides_by(N, D),
%     check_divisors(N, D+1).


% is_prime(1) :- !, false.
% is_prime(2) :- !.
% is_prime(N) :-
%     N > 2,
%     check_divisors(N, 2).
% is_prime(N, 1) :- is_prime(N).
% is_prime(N, 0) :- \+ is_prime(N).


% ...which won't work with CLP(FD) without a fair bit of work:
% https://stackoverflow.com/questions/39591888/clpfd-constraint-is-a-prime-number

% ...so we'll stick with this until it comes back to bite us
%is_prime(N) :- N in {2, 3, 5, 7, 11, 13, 17, 19}.

is_prime(Expression) :-
    (   integer(Expression)
    ->  N = Expression
    ;   number(Expression)
    ->  N is round(Expression)
    ;   N is Expression          % evaluate arithmetic term
    ),
    N > 1,
    Limit is floor(sqrt(N)),      % <-- evaluate here
    \+ ( between(2, Limit, D), N mod D =:= 0 ).

is_prime(N, 1) :- is_prime(N).
is_prime(N, 0) :- \+ is_prime(N).

is_square(N) :-
    N #= _^2.

is_odd(N) :-
    N mod 2 #= 1.

is_even(N) :-
    N mod 2 #= 0.

divides_by(X, Y) :-
    X mod Y =:= 0.

occurrenceof([], _, 0).
occurrenceof([H|T], X, N) :-
    occurrenceof(T, X, N0),
    (   H = X
    ->  N is N0 + 1
    ;   N = N0
    ).

solution_count(Predicate, Count) :-
    Goal =.. [Predicate, A, B, C, D],
    findall([A, B, C, D], Goal, Solutions),
    length(Solutions, Count).
