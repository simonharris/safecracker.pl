:- use_module('../safe_cracker').
:- use_module('../parser').


solution_20250511(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. Exactly one of the digits is one
    % occurrenceof(Vs, 1, 1),
    apply_clue('Exactly one of the digits is one', Vs),

    % 2. The first and third total 13
    % A + C #= 13,
    apply_clue('The first and third total 13', Vs),

    % 3. The third and fourth differ by two
    % abs(C - D) #= 2,
    apply_clue('The third and fourth differ by two', Vs),

    % 4. The first is greater than the fourth
    % A #> D,
    apply_clue('The first is greater than the fourth', Vs),

    % 5. Exactly two digits are divisible by three
    % maplist(divides_by_3, Vs, Threes),
    % sum(Threes, #=, 2),
    apply_clue('Exactly two digits are divisible by three', Vs),

    label(Vs).


divides_by_3(N, B) :- (N mod 3 #= 0) #<==> B.
