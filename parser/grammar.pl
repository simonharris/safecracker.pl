
parse_clue(Sentence, Clue) :-
    phrase(clue(Clue), Sentence).

% The third digit is less than 5
%  clue --> digit, i, op, num.
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


%% ----------------------------------------------------------------------------

/*



digit(Ordinal) -->
    det(the),
    ord(Ordinal),
    d(digit).

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].

op(less_than) --> ['less', 'than'].
op(greater_than) --> ['greater', 'than'].
op(equal_to) --> ['equal', 'to'].

num(Num) -->
    [Num],
    { atom_number(Num, _NumAsNumber) }.

?- phrase(clue(Ordinal, Operator, Num), ['the', 'second', 'digit', 'is', 'less', 'than', '7']).
Ordinal = second,
Operator = less_than,
Num = '7'.


One approach is to define a top-level grammar rule that covers all possible types of clues. For example:
Prolog
clue(Clue) -->
    (   ordinal_clue(Clue)
    ;   other_clue_type(Clue)
    ;   another_clue_type(Clue)
    ).
Then, you can define separate grammar rules for each type of clue:
Prolog
ordinal_clue(clue(Ordinal, Operator, Num)) -->
    digit(Ordinal),
    i(Is),
    op(Operator),
    num(Num).
When you call phrase/2, you can leave the type of clue unspecified:
Prolog
?- phrase(clue(Clue), ['the', 'second', 'digit', 'is', 'less', 'than', '7']).

*/
