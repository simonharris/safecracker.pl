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

% eg. The third digit is less than five
clue(clue(Ordinal, Operator, Number)) -->
    position(Ordinal),
    be,
    operator(Operator),
    safe_digit(Number),
    !.
% eg. The third digit is less than the second
% eg. The second is twice the fourth
clue(clue(Ordinal1, Operator, Ordinal2)) -->
    position(Ordinal1),
    be,
    operator(Operator),
    position(Ordinal2),
    !.
% eg. The second digit is odd
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
% eg. The fourth is greater than the sum of the second and third
clue(clue(Ordinal2,  Ordinal3, add_up_to_less_than, Ordinal1)) -->
    position(Ordinal1),
    be,
    gt,
    sumof,
    position(Ordinal2),
    and,
    position(Ordinal3),
    !.
% eg. The third and fourth differ by two
% eg. The first and third total 13
clue(clue(Ordinal1, Ordinal2, Func, Howmany)) -->
    position(Ordinal1),
    and,
    position(Ordinal2),
    function(Func),
    numeric_string(Howmany),
    !.
% eg. The fourth is three more than the first
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
% eg. Exactly one of the digits is one
clue(clue(Outcome, Howmany, Value)) -->
    quant(Howmany),
    outcome(Outcome),
    safe_digit(Value),
    !.
% eg. The sum of the second and third is a square
clue(clue(sum, Ordinal1, Ordinal2, Adj)) -->
    sumof,
    position(Ordinal1),
    and,
    position(Ordinal2),
    be,
    det,
    adj(Adj),
    !.
% eg. Either the second or the third is odd, but not both
clue(clue(either, Ordinal1, Ordinal2, Adj)) -->
    either,
    position(Ordinal1),
    or,
    position(Ordinal2),
    be,
    adj(Adj),
    superfluous_waffle,
    !.
% eg. The second exceeds the first by more than two
% We're deep into the outliers here, let's just get it out the way
clue(clue(Ordinal1, Ordinal2, exceeds_by_more_than, Howmany)) -->
    position(Ordinal1),
    exceeds,
    position(Ordinal2),
    by,
    more,
    than,
    numeric_string(Howmany),
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
det --> ['a'].

and --> ['and'].
or --> ['or'].

be --> ['is'].
be --> ['are'].

outcome(divisible_by) --> be, [ 'divisible', 'by'].
outcome(equal) --> be.
out(Out) --> [Out], { member(Out, ['divisible_by', 'equal']) }.

ord(first) --> ['first'].
ord(second) --> ['second'].
ord(third) --> ['third'].
ord(fourth) --> ['fourth'].

gt --> ['greater', 'than'].

function(differ_by) --> ['differ', 'by'].
function(add_up_to) --> ['total'].
function(greater_than) --> ['more', 'than'].
function(greater_than) --> gt.
function(less_than) --> ['less', 'than'].
fun(Fun) -->
    [Fun], { member(Fun, ['differ_by', 'add_up_to', 'greater_than',
                            'less_than', 'add_up_to_less_than',
                            'exceeds_by_more_than']) }.

operator(less_than) --> ['less', 'than'].
operator(greater_than) --> ['greater', 'than'].
operator(twice) --> ['twice'].

sumof --> ['the', 'sum', 'of'].

either --> ['either'].

safe_digit(D) --> [C], { member(C, ['1','2','3','4','5','6','7','8','9']), atom_number(C, D) }.

numeric_string(Num) --> [Num]. % yeah, you try doing better

superfluous_waffle --> ['but', 'not', 'both'].
superfluous_waffle --> [].

exceeds --> ['exceeds'].
by --> ['by'].
more --> ['more'].
than --> ['than'].

