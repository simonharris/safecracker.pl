:- use_module('safe_cracker').
:- use_module('parser').


solution(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    forall(clue(Line),
       apply_clue(Line, Vs)),
    label(Vs).


solution_count(A, B, C, D, Count) :-
    findall(t, solution(A, B, C, D), Solutions),
    length(Solutions, Count).
