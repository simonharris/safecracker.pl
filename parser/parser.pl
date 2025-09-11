:- module(parser, [
    apply_clue/2,
    % for testing,
    atoms_clue/2,
    clue_constraint/3,
    normalise_numbers/2,
    text_preprocessed/2

]).
:- use_module(library(clpfd)).
:- use_module('grammar').
:- use_module('constraint_factories').


apply_clue(Text, Vs) :-
    text_preprocessed(Text, Atoms),
    % writeln(Atoms),
    atoms_constraint_(Atoms, Constraint, Vs),
    % writeln(Constraint),
    call(Constraint).

text_preprocessed(Text, Processed) :-
    strip_commas(Text, CleanText),
    split_string(CleanText, " ", "", TextList),
    maplist(atom_string, Atoms, TextList),
    maplist(downcase_atom, Atoms, AtomsLower),
    maplist(normalise_numbers, AtomsLower, Processed),
    !.

atoms_constraint_(ClueAtoms, Constraint, Vars) :-
    % turn the text into a clue object
    atoms_clue(ClueAtoms, Clue),
    % turn the clue into a constraint object
    clue_constraint(Clue, Vars, Constraint).

% actually call the grammar
atoms_clue(Sentence, Clue) :-
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
% eg. The third digit is square
clue_constraint(clue(Position, Adj), Vars, Constraint) :-
    var_for_position(Position, Vars, Var),
    adjective_val(Adj),
    adjective_constraint(Adj, Vars, Var, Constraint).

/*

two digits

*/

% eg. The first and second total the third
clue_constraint(clue(Position1, Position2, Func, Position3), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    fun_val(Func),
    var_for_position(Position3, Vars, Var3),
    sum_rel_constraint(eq, Var1, Var2, Var3, Constraint),
    !.
% eg. The third and fourth differ by two
% eg. The first and third total 13
clue_constraint(clue(Position1, Position2, Func, Howmany), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    fun_val(Func),
    function_constraint(Func, Var1, Var2, Howmany, Constraint),
    !.
% eg. The sum of the second and third is a square
clue_constraint(clue(sum, Position1, Position2, square), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    boutcome_constraint(sum, Var1, Var2, square, Constraint),
    !.
% eg. The sum of the second and third is a two digit prime. nb duplication with above
clue_constraint(clue(sum, Position1, Position2, two_digit_prime), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    boutcome_constraint(sum, Var1, Var2, two_digit_prime, Constraint),
    !.
% eg. The sum of the first and third exceeds 10
clue_constraint(clue(sum, greater_than, Position1, Position2, Howmany), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    sum_rel_constraint(greater_than, Var1, Var2, Howmany, Constraint),
    !.
% eg. The sum of the first and second is less than the fourth
clue_constraint(clue(sum, less_than, Position1, Position2, Position3), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    var_for_position(Position3, Vars, Var3),
    sum_rel_constraint(less_than, Var1, Var2, Var3, Constraint),
    !.
% eg. The sum of the first and second is less than seven
clue_constraint(clue(sum, less_than, Position1, Position2, Howmany), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    sum_rel_constraint(less_than, Var1, Var2, Howmany, Constraint),
    !.
% eg. The sum of the second and fourth is divisible by five
% nb. TODO: Could probably generalise with the above
clue_constraint(clue(sum, db, Position1, Position2, Howmany), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    sum_rel_constraint(db, Var1, Var2, Howmany, Constraint),
    !.
clue_constraint(clue(sum, equals, Position1, Position2, Howmany), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    sum_rel_constraint(eq, Var1, Var2, Howmany, Constraint),
    !.

/*

three digits

*/

% eg. The fourth is greater than the sum of the second and third
clue_constraint(clue(sum, less_than, Position1, Position2, Position3), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    var_for_position(Position3, Vars, Var3),
    sum_rel_constraint(less_than, Var1, Var2, Var3, Constraint),
    !.
% eg. The second minus the first is less than three
clue_constraint(clue(minus, less_than, Position1, Position2, Howmany), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    minus_rel_constraint(less_than, Var1, Var2, Howmany, Constraint),
    !.


/*

exactly how many digits

*/

% eg. Only one digit is odd
% eg. Exactly two digits are not prime
clue_constraint(clue(Adj, Howmany), Vars, Constraint) :-
    adjective_val(Adj),
    qadj_constraint(Adj, Vars, Howmany, Constraint),
    !.
% eg. Exactly one of the digits is one
% eg. Exactly two digits are divisible by three
clue_constraint(clue(Outcome, Howmany, Value), Vars, Constraint) :-
    outcome_val(Outcome),
    qoutcome_constraint(Outcome, Vars, Howmany, Value, Constraint),
    !.

/*

Odds and ends

*/

% eg. Either the second or the third is odd, but not both
clue_constraint(clue(either, Position1, Position2, Adj), Vars, Constraint) :-
    var_for_position(Position1, Vars, Var1),
    var_for_position(Position2, Vars, Var2),
    either_constraint(Adj, Var1, Var2, Constraint),
    !.


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

normalise_numbers(Atom, Number) :-
    atom_number(Atom, Number).
normalise_numbers('one', 1).
normalise_numbers('two', 2).
normalise_numbers('three', 3).
normalise_numbers('four', 4).
normalise_numbers('five', 5).
normalise_numbers('six', 6).
normalise_numbers('seven', 7).
normalise_numbers('eight', 8).
normalise_numbers('nine',  9).
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
            'differ_by_less_than',
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
