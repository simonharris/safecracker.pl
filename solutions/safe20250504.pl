:- use_module('../safe_cracker').


solution_20250504(A, B, C, D) :-
    Vs = [A, B, C, D],
    common_constraints(Vs),

    % 1. The third digit is less than 5
    C #< 5,

    % 2. The second is twice the fourth
    B #= 2 * D,

    % 3. Exactly two digits are prime
    maplist(is_prime, Vs, PrimeDigits),
    sum(PrimeDigits, #=, 2),

    % 4. The second exceeds the first by more than 2
    B #> A + 2,

    % 5. The first and third differ by 3
    abs(A - C) #= 3,

    label(Vs).


% count_primes([], 0).
% count_primes([H|T], Count) :-
%     count_primes(T, CountT),
%     (   is_prime(H)
%     ->  Count #= CountT + 1
%     ;   Count #= CountT
%     ).

% exactly_two_primes(List) :-
%     count_primes(List, 2).