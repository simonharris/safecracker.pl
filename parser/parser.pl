:- module(parser, [
    apply_clue/2,
    % for testing
    normalise_numbers/2,
    parse_clue/2
]).
:- use_module(library(clpfd)).
:- use_module('grammar').


apply_clue(Text, Vs) :-
    parse_text(Text, Vs, Constraint),
    % writeln(Constraint),
    call(Constraint).


parse_text(Text, Vars, Constraint) :-
    % pre-processing
    strip_commas(Text, CleanText),
    split_string(CleanText, " ", "", TextList),
    maplist(atom_string, Atoms, TextList),
    maplist(downcase_atom, Atoms, AtomsLower),
    maplist(normalise_numbers, AtomsLower, AtomsWithnumbers),
    % turn the text into a clue object
    parse_clue(AtomsWithnumbers, Clue),
    % turn the clue into a constraint object
    clue_constraint(Clue, Vars, Constraint).


parse_clue(Sentence, Clue) :-
    phrase(clue_spec(Clue), Sentence).


%% map clues to constraint types ----------------------------------------------

/*

one digit

*/

% eg. The third digit is less than five
clue_constraint(clue(Position, Relation, Num), Vars, Constraint) :-
    var_for_position(Position, Vars, Var),
    safe_digit_val(Num), % this is a bit fragile
    relation_constraint(Relation, Var, Num, Constraint).
% eg. The third digit is less than the second
% eg. The second is twice the fourth
clue_constraint(clue(Position1, Relation, Position2), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    relation_constraint(Relation, Var1, Var2, Constraint).
% eg. The second digit is odd
clue_constraint(clue(Position, Adj), Vars, Constraint) :-
    var_for_position(Position, Vars, Var),
    adjective_val(Adj),
    adjective_constraint(Adj, Vars, Var, Constraint).

/*

two digits --------------------------------------------------------------------

*/

% eg. The third and fourth differ by two
% eg. The first and third total 13
clue_constraint(clue(Position1, Position2, Func, HowmanyStr), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    fun_val(Func),
    atom_number(HowmanyStr, Howmany),
    function_constraint(Func, Var1, Var2, Howmany, Constraint),
    !.
% eg. The sum of the second and third is a square
clue_constraint(clue(sum, Position1, Position2, square), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    boutcome_constraint(sum, Var1, Var2, square, Constraint),
    !.
% eg. The sum of the first and third exceeds 10
clue_constraint(clue(sum, gt, Position1, Position2, HowmanyStr), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    atom_number(HowmanyStr, Howmany),
    sum_rel_constraint(gt, Var1, Var2, Howmany, Constraint),
    !.
% eg. The sum of the second and fourth is divisible by five'
% nb. Could probably generalise with the above
clue_constraint(clue(sum, db, Position1, Position2, HowmanyStr), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    atom_number(HowmanyStr, Howmany),
    sum_rel_constraint(db, Var1, Var2, Howmany, Constraint),
    !.
% eg. The second minus the first is less than three
clue_constraint(clue(minus, lt, Position1, Position2, HowmanyStr), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    atom_number(HowmanyStr, Howmany),
    minus_rel_constraint(lt, Var1, Var2, Howmany, Constraint),
    !.

/*

three digits ------------------------------------------------------------------

*/

% eg. The fourth is greater than the sum of the second and third
% TODO: generalise with the one above (see #10)
clue_constraint(clue(sum, lt, Position1, Position2, Position3), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    var_for_position(Position3, Vars, Var3),
    sum_rel_constraint(lt, Var1, Var2, Var3, Constraint),
    !.

% eg. The first and second total the third % TODO: should be a sum_rel?
clue_constraint(clue(Position1, Position2, Func, Position3), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    fun_val(Func),
    var_for_position(Position3, Vars, Var3),
    function_constraint(Func, Var1, Var2, Var3, Constraint),
    !.

/*

exactly how many digits -------------------------------------------------------

*/

% eg. Only one digit is odd
% eg. Exactly two digits are not prime
clue_constraint(clue(Adj, HowmanyStr), Vars, Constraint) :-
    adjective_val(Adj),
    atom_number(HowmanyStr, Howmany),
    qadj_constraint(Adj, Vars, Howmany, Constraint),
    !.
% eg. Exactly one of the digits is one
% eg. Exactly two digits are divisible by three
clue_constraint(clue(Outcome, HowmanyStr, Value), Vars, Constraint) :-
    outcome_val(Outcome),
    atom_number(HowmanyStr, Howmany),
    qoutcome_constraint(Outcome, Vars, Howmany, Value, Constraint),
    !.

/*

Odds and ends -----------------------------------------------------------------

*/

% eg. Either the second or the third is odd, but not both
clue_constraint(clue(either, Position1, Position2, Adj), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    either_constraint(Adj, Var1, Var2, Constraint),
    !.


%% constraint factories -------------------------------------------------------


relation_constraint(less_than, A, B, A #< B).
relation_constraint(greater_than, A, B, A #> B).
relation_constraint(divisible_by, A, B, divides_by(A, B)).
relation_constraint(twice, A, B, A #= B*2).

adjective_constraint(odd, _, Var1, is_odd(Var1)).
adjective_constraint(even, _, Var1, is_even(Var1)).
adjective_constraint(prime, _, Var1, is_prime(Var1, 1)).
adjective_constraint(greatest, Vars, Var1, (max_list(Vars, Biggest), Biggest #= Var1)).

function_constraint(differ_by, Var1, Var2, Howmany, abs(Var1 - Var2) #= Howmany).
function_constraint(differ_by_more_than, Var1, Var2, Howmany, abs(Var1 - Var2) #> Howmany).
function_constraint(differ_by_no_more_than, Var1, Var2, Howmany, abs(Var1 - Var2) #=< Howmany).
function_constraint(add_up_to, Var1, Var2, Howmany, (Var1 + Var2) #= Howmany).
function_constraint(less_than, Var1, Var2, Howmany, (Var2 - Var1) #= Howmany).
function_constraint(greater_than, Var1, Var2, Howmany, (Var1 - Var2) #= Howmany).
function_constraint(exceeds_by_more_than, Var1, Var2, Howmany, Var1 #> (Var2 + Howmany)).

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

% Rhs can be a column variable or a number literal
sum_rel_constraint(gt, Var1, Var2, Rhs, (Var1 + Var2) #> Rhs).
sum_rel_constraint(lt, Var1, Var2, Rhs, (Var1 + Var2) #< Rhs).
sum_rel_constraint(eq, Var1, Var2, Rhs, (Var1 + Var2) #= Rhs).
sum_rel_constraint(db, Var1, Var2, Rhs, divides_by((Var1 + Var2), Rhs)).

minus_rel_constraint(gt, Var1, Var2, Rhs, (Var1 - Var2) #> Rhs).
minus_rel_constraint(lt, Var1, Var2, Rhs, (Var1 - Var2) #< Rhs).
minus_rel_constraint(eq, Var1, Var2, Rhs, (Var1 - Var2) #= Rhs).

either_constraint(odd, Var1, Var2, xor(Var1 mod 2 #= 1, Var2 mod 2 #= 1)).


%% utils ----------------------------------------------------------------------


strip_commas(String, Stripped) :-
    atom_chars(String, Chars),
    exclude(=(','), Chars, StrippedChars),
    atomics_to_string(StrippedChars, Stripped).

% nb safe_cracker/divides_by/2
divisible_by(Divisor, X, B) :- (X mod Divisor #= 0) #<==> B.

position_index(first, 1).
position_index(second, 2).
position_index(third, 3).
position_index(fourth, 4).

var_for_position(Position, Vars, Var) :-
    position_index(Position, Index),
    nth1(Index, Vars, Var).

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


% Wrappers etc for DCG predicates in grammar.pl -------------------------------


safe_digit_val(Num) :-
    phrase(safe_digit(Num), [_]).

adjective_val(Adj) :-
    phrase(adj(Adj), [_]).

fun_val(Func) :-
    member(Func,
        [
            'differ_by',
            'differ_by_more_than',
            'differ_by_no_more_than',
            'add_up_to',
            'greater_than',
            'less_than',
            'add_up_to_less_than',
            'exceeds_by_more_than'
        ]
    ).

outcome_val(Outcome) :-
    phrase(out(Outcome), [_]).
