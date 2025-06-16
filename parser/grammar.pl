:- use_module(library(clpfd)).

position_index(first, 1).
position_index(second, 2).
position_index(third, 3).
position_index(fourth, 4).


parse_text(Text, Vars, Constraint) :-
    split_string(Text, " ", "", TextList),
    maplist(atom_string, Atoms, TextList),
    % writeln(Atoms),
    parse_clue(Atoms, Clue),
    % writeln(Clue),
    clue_constraint(Clue, Vars, Constraint).

parse_clue(Sentence, Clue) :-
    phrase(clue(Clue), Sentence).


clue_constraint(clue(Position, less_than, Num), Vars, Constraint) :-
    safe_digit_val(Num),
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    Constraint = (Var #< Num).
clue_constraint(clue(Position, greater_than, Num), Vars, Constraint) :-
    safe_digit_val(Num),
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    Constraint = (Var #> Num).
clue_constraint(clue(Position1, less_than, Position2), Vars, Constraint) :-
    position_val(Position2),
    position_index(Position1, Index1),
    position_index(Position2, Index2),
    nth1(Index1, Vars, Var1),
    nth1(Index2, Vars, Var2),
    Constraint = (Var1 #< Var2).


% eg. the third digit is less than 5
clue(clue(Ordinal, Operator, Number)) -->
    digit(Ordinal),
    i,
    operator(Operator),
    safe_digit(Number),
    !.
clue(clue(Ordinal, Operator, Arg2)) -->
    digit(Ordinal),
    i,
    operator(Operator),
    digit(Arg2),
    !.

position_val(Token) :-
    phrase(ord(Token), [_]).

% TODO: rename eg. position
digit(Ordinal) --> det, ord(Ordinal).
digit(Ordinal) --> det, ord(Ordinal), d.

det --> ['the'].
det --> ['The'].

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].

d --> ['digit'].

i --> ['is'].

operator(less_than) --> [less, than].
operator(greater_than) --> [greater, than].
operator(equal_to) --> [equal, to].

% safe_digit_val(Num) :- member(Num, [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]).
safe_digit_val(Num) :- phrase(safe_digit(Num), [_]).

safe_digit(1) --> ['1'].
safe_digit(2) --> ['2'].
safe_digit(3) --> ['3'].
safe_digit(4) --> ['4'].
safe_digit(5) --> ['5'].
safe_digit(6) --> ['6'].
safe_digit(7) --> ['7'].
safe_digit(8) --> ['8'].
safe_digit(9) --> ['9'].
