
position_index(first, 1).
position_index(second, 2).
position_index(third, 3).
position_index(fourth, 4).


parse_clue(Sentence, Clue) :-
    phrase(clue(Clue), Sentence).


clue_constraint(clue(Position, less_than, Num), Vars, Constraint) :-
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    Constraint = (Var #< Num).
clue_constraint(clue(Position, greater_than, Num), Vars, Constraint) :-
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    Constraint = (Var #> Num).
clue_constraint(clue(Position, equal_to, Num), Vars, Constraint) :-
    position_index(Position, Index),
    nth1(Index, Vars, Var),
    Constraint = (Var #= Num).


% eg. the third digit is less than 5
clue(clue(Ordinal, Operator, Number)) -->
    digit(Ordinal),
    i,
    operator(Operator),
    safe_digit(Number).

digit(Ordinal) --> det, ord(Ordinal).
digit(Ordinal) --> det, ord(Ordinal), d.

det --> ['the'].

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].

d --> ['digit'].

i --> ['is'].

operator(less_than) --> [less, than].
operator(greater_than) --> [greater, than].
operator(equal_to) --> [equal, to].

safe_digit(1) --> ['1'].
safe_digit(2) --> ['2'].
safe_digit(3) --> ['3'].
safe_digit(4) --> ['4'].
safe_digit(5) --> ['5'].
safe_digit(6) --> ['6'].
safe_digit(7) --> ['7'].
safe_digit(8) --> ['8'].
safe_digit(9) --> ['9'].
