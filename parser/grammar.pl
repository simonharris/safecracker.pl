:- module(grammar, [
    clue//1,
    adj//1,
    fun//1,
    operator//1,
    ord//1,
    out//1,
    position//1,
    safe_digit//1
]).

% eg. the third digit is less than 5
clue(clue(Ordinal, Operator, Number)) -->
    position(Ordinal),
    be,
    operator(Operator),
    safe_digit(Number),
    !.
% eg. the third digit is less than the second
% eg. the second is twice the fourth
clue(clue(Ordinal1, Operator, Ordinal2)) -->
    position(Ordinal1),
    be,
    operator(Operator),
    position(Ordinal2),
    !.
% eg. the second digit is odd
clue(clue(Ordinal, Adj)) -->
    position(Ordinal),
    be,
    adj(Adj),
    !.
% eg. The first and second total the third
clue(clue(Ordinal1, Ordinal2, Func, Ordinal3)) -->
    position(Ordinal1),
    and,
    position(Ordinal2),
    function(Func),
    position(Ordinal3),
    !.
% eg. the third and fourth differ by 2
% eg. the first and third total 13
clue(clue(Ordinal1, Ordinal2, Func, Howmany)) -->
    position(Ordinal1),
    and,
    position(Ordinal2),
    function(Func),
    numeric_string(Howmany),
    !.
% eg. The fourth is 3 more than the first
clue(clue(Ordinal1, Ordinal2, Func, Howmany)) -->
    position(Ordinal1),
    be,
    numeric_string(Howmany),
    function(Func),
    position(Ordinal2),
    !.
% eg. Exactly three digits are even
clue(clue(Adj, Howmany)) -->
    quant(Howmany),
    be,
    adj(Adj),
    !.
% eg. Exactly one of the digits is 1
clue(clue(Outcome, Howmany, Value)) -->
    quant(Howmany),
    outcome(Outcome),
    safe_digit(Value),
    !.


quant(Howmany) -->
    qmod,
    numeric_string(Howmany),
    part,
    d.

qmod --> ['exactly'].
qmod --> ['only'].
qmod --> [].

part --> ['of', 'the'].
part --> [].

d --> ['digit'].
d --> ['digits'].

adj(Adj) --> [Adj], { member(Adj, ['prime', 'odd', 'even', 'square']) }.

position(Ordinal) --> det, ord(Ordinal), d.
position(Ordinal) --> det, ord(Ordinal).
position(Ordinal) --> ord(Ordinal).

det --> ['the'].
and --> ['and'].

be --> ['is'].
be --> ['are'].

outcome(divisible_by) --> be, [ 'divisible', 'by'].
outcome(equal) --> be.
out(Out) --> [Out], { member(Out, ['divisible_by', 'equal']) }.

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].

function(differ_by) --> ['differ', 'by'].
function(add_up_to) --> ['total'].
function(greater_than) --> ['more', 'than'].
function(less_than) --> ['less', 'than'].
fun(Fun) --> [Fun], { member(Fun, ['differ_by', 'add_up_to', 'greater_than', 'less_than']) }.

operator(less_than) --> ['less', 'than'].
operator(greater_than) --> ['greater', 'than'].
operator(twice) --> ['twice'].
% operator(equal_to) --> ['equal', 'to'].

safe_digit(D) --> [C], { member(C, ['1','2','3','4','5','6','7','8','9']), atom_number(C, D) }.

numeric_string(Num) --> [Num]. % yeah, you try doing better

