:- module(constraint_factories, [
    adjective_constraint/4,
    either_constraint/4,
    function_constraint/5,
    qadj_constraint/4,
    boutcome_constraint/5,
    qoutcome_constraint/5,
    minus_rel_constraint/5,
    sum_rel_constraint/5,
    relation_constraint/4
]).
:- use_module(library(clpfd)).


adjective_constraint(odd, _, Var1, is_odd(Var1)).
adjective_constraint(even, _, Var1, is_even(Var1)).
adjective_constraint(prime, _, Var1, is_prime(Var1)).
adjective_constraint(square, _, Var1, is_square(Var1)).
adjective_constraint(greatest, Vars, Var1, (max_list(Vars, Biggest), Biggest #= Var1)).

function_constraint(differ_by, Var1, Var2, Howmany, abs(Var1 - Var2) #= Howmany).
function_constraint(differ_by_less_than, Var1, Var2, Howmany, abs(Var1 - Var2) #< Howmany).
function_constraint(differ_by_more_than, Var1, Var2, Howmany, abs(Var1 - Var2) #> Howmany).
function_constraint(differ_by_no_more_than, Var1, Var2, Howmany, abs(Var1 - Var2) #=< Howmany).
function_constraint(add_up_to, Var1, Var2, Howmany, (Var1 + Var2) #= Howmany).
function_constraint(less_than, Var1, Var2, Howmany, (Var2 - Var1) #= Howmany).
function_constraint(greater_than, Var1, Var2, Howmany, (Var1 - Var2) #= Howmany).
function_constraint(exceeds_by_more_than, Var1, Var2, Howmany, Var1 #> (Var2 + Howmany)).

relation_constraint(less_than, A, B, A #< B).
relation_constraint(greater_than, A, B, A #> B).
relation_constraint(divisible_by, A, B, divides_by(A, B)).
relation_constraint(twice, A, B, A #= B*2).

qadj_constraint(odd, Vars, Howmany, (include(is_odd, Vars, Odds), length(Odds, Howmany))).
qadj_constraint(even, Vars, Howmany, (include(is_even, Vars, Odds), length(Odds, Howmany))).
qadj_constraint(square, Vars, Howmany, (include(is_square, Vars, Odds), length(Odds, Howmany))).
qadj_constraint(prime, Vars, Howmany, ( maplist(is_prime, Vars, PrimeDigits), sum(PrimeDigits, #=, Howmany))).
qadj_constraint(not_prime, Vars, Howmany, ( maplist(is_prime, Vars, PrimeDigits), sum(PrimeDigits, #=, (4-Howmany)))).

qoutcome_constraint(equal, Vars, Howmany, Value, occurrenceof(Vars, Howmany, Value)).
qoutcome_constraint(divisible_by, Vars, Howmany, Divisor,
                   (maplist(divisible_by(Divisor), Vars, Bs),
                    sum(Bs, #=, Howmany))).

boutcome_constraint(sum, Var1, Var2, square, is_square(Var1 + Var2)).
boutcome_constraint(sum, Var1, Var2, prime, is_prime(Var1 + Var2)).
boutcome_constraint(sum, Var1, Var2, two_digit_prime, (is_prime(Var1 + Var2), ((Var1+Var2) #>= 10))).

% Rhs can be a column variable or a number literal
minus_rel_constraint(greater_than, Var1, Var2, Rhs, (Var1 - Var2) #> Rhs).
minus_rel_constraint(less_than, Var1, Var2, Rhs, (Var1 - Var2) #< Rhs).
minus_rel_constraint(eq, Var1, Var2, Rhs, (Var1 - Var2) #= Rhs).

sum_rel_constraint(greater_than, Var1, Var2, Rhs, (Var1 + Var2) #> Rhs).
sum_rel_constraint(less_than, Var1, Var2, Rhs, (Var1 + Var2) #< Rhs).
sum_rel_constraint(eq, Var1, Var2, Rhs, (Var1 + Var2) #= Rhs).
sum_rel_constraint(db, Var1, Var2, Rhs, divides_by((Var1 + Var2), Rhs)).

either_constraint(odd, Var1, Var2, xor(Var1 mod 2 #= 1, Var2 mod 2 #= 1)).
