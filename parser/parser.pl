:- module(parser, [
    apply/2,
    % for testing
    normalise_numbers/2,
    parse_clue/2
]).
:- use_module(library(clpfd)).
:- use_module('grammar').


apply(Text, Vs) :-
    parse_text(Text, Vs, Constraint),
    call(Constraint).

parse_text(Text, Vars, Constraint) :-
    % pre-processing
    split_string(Text, " ", "", TextList),
    maplist(atom_string, Atoms, TextList),
    maplist(downcase_atom, Atoms, AtomsLower),
    maplist(normalise_numbers, AtomsLower, AtomsWithnumbers),
    % turn the text into a clue object
    parse_clue(AtomsWithnumbers, Clue),
    % turn the clue into a constraint object
    clue_constraint(Clue, Vars, Constraint).

parse_clue(Sentence, Clue) :-
    phrase(clue(Clue), Sentence).

% eg. the third digit is less than 5
clue_constraint(clue(Position, Relation, Num), Vars, Constraint) :-
    position_val(Position),
    safe_digit_val(Num),
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    relation_constraint(Relation, Var, Num, Constraint).
% eg. the third digit is less than the second
% eg. the second is twice the fourth
clue_constraint(clue(Position1, Relation, Position2), Vars, Constraint) :-
    position_val(Position1),
    position_val(Position2),
    position_index(Position1, Index1),
    position_index(Position2, Index2),
    nth1(Index1, Vars, Var1),
    nth1(Index2, Vars, Var2),
    relation_constraint(Relation, Var1, Var2, Constraint).
% eg. the second digit is odd
clue_constraint(clue(Position, Adj), Vars, Constraint) :-
    position_val(Position),
    adjective_val(Adj),
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    adjective_constraint(Adj, Var, Constraint).
% eg. The third and fourth differ by 2
% eg. The first and third total 13
clue_constraint(clue(Position1, Position2, Func, HowmanyStr), Vars, Constraint) :-
    position_val(Position1),
    position_val(Position2),
    fun_val(Func),
    position_index(Position1, Index1),
    position_index(Position2, Index2),
    nth1(Index1, Vars, Var1),
    nth1(Index2, Vars, Var2),
    atom_number(HowmanyStr, Howmany),
    function_constraint(Func, Var1, Var2, Howmany, Constraint),
    !.
% eg. The first and second total the third
clue_constraint(clue(Position1, Position2, Func, Position3), Vars, Constraint) :-
    position_val(Position1),
    position_val(Position2),
    position_val(Position3),
    fun_val(Func),
    position_index(Position1, Index1),
    position_index(Position2, Index2),
    position_index(Position3, Index3),
    nth1(Index1, Vars, Var1),
    nth1(Index2, Vars, Var2),
    nth1(Index3, Vars, Var3),
    function_constraint(Func, Var1, Var2, Var3, Constraint),
    !.
% eg. Only one digit is odd
clue_constraint(clue(Adj, HowmanyStr), Vars, Constraint) :-
    adjective_val(Adj),
    atom_number(HowmanyStr, Howmany),
    qadj_constraint(Adj, Vars, Howmany, Constraint),
    !.
% eg. Exactly one of the digits is 1
% eg. Exactly two digits are divisible by three
clue_constraint(clue(Outcome, HowmanyStr, Value), Vars, Constraint) :-
    outcome_val(Outcome),
    atom_number(HowmanyStr, Howmany),
    qoutcome_constraint(Outcome, Vars, Howmany, Value, Constraint),
    % writeln(Constraint),
    !.

relation_constraint(less_than, A, B, A #< B).
relation_constraint(greater_than, A, B, A #> B).
relation_constraint(twice, A, B, A #= B*2).

adjective_constraint(odd, Var, is_odd(Var)).
adjective_constraint(even, Var, is_even(Var)).
adjective_constraint(prime, Var, is_prime(Var, 1)).

qadj_constraint(odd, Vars, Howmany, (include(is_odd, Vars, Odds), length(Odds, Howmany))).
qadj_constraint(even, Vars, Howmany, (include(is_even, Vars, Odds), length(Odds, Howmany))).
qadj_constraint(square, Vars, Howmany, (include(is_square, Vars, Odds), length(Odds, Howmany))).
qadj_constraint(prime, Vars, Howmany, ( maplist(is_prime, Vars, PrimeDigits), sum(PrimeDigits, #=, Howmany))).

qoutcome_constraint(equal, Vars, Howmany, Value, occurrenceof(Vars, Howmany, Value)).
qoutcome_constraint(divisible_by, Vars, Howmany, Divisor,
                   (maplist(divisible_by(Divisor), Vars, Bs),
                    sum(Bs, #=, Howmany))).

divisible_by(Divisor, X, B) :- (X mod Divisor #= 0) #<==> B.

function_constraint(differ_by, Var1, Var2, Howmany, abs(Var1 - Var2) #= Howmany).
function_constraint(add_up_to, Var1, Var2, Howmany, (Var1 + Var2) #= Howmany).
function_constraint(less_than, Var1, Var2, Howmany, (Var2 - Var1) #= Howmany).
function_constraint(greater_than, Var1, Var2, Howmany, (Var1 - Var2) #= Howmany).

position_index(first, 1).
position_index(second, 2).
position_index(third, 3).
position_index(fourth, 4).

normalise_numbers('one', '1').
normalise_numbers('two', '2').
normalise_numbers('three', '3').
normalise_numbers('four', '4').
normalise_numbers('five', '5').
normalise_numbers('six', '6').
normalise_numbers('seven', '7').
normalise_numbers('eight', '8').
normalise_numbers('nine', '9').
normalise_numbers(Atom, Atom).


% Wrappers for DCG predicates in grammar.pl -----------------------------------


position_val(Token) :-
    phrase(ord(Token), [_]).

safe_digit_val(Num) :-
    phrase(safe_digit(Num), [_]).

adjective_val(Adj) :-
    phrase(adj(Adj), [_]).

fun_val(Func) :-
    phrase(fun(Func), [_]).

outcome_val(Outcome) :-
    phrase(out(Outcome), [_]).
