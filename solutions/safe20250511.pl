:- use_module('../safe_cracker').


solution_20250511(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. Exactly one of the digits is 1
    occurrenceof(Vs, 1, 1),

    % 2. The first and third total 13
    A + C #= 13,

    % 3. The third and fourth differ by 2
    abs(C - D) #= 2,

    % 4. The first is greater than the fourth
    A #> D,

    % 5. Exactly two digits are divisible by 3
    maplist(divides_by_3, Vs, Threes),
    sum(Threes, #=, 2),

    label(Vs).


divides_by_3(N, B) :- (N mod 3 #= 0) #<==> B.

% findall([A, B, C, D], solution_20250511(A, B, C, D), Solutions), length(Solutions, Count).
