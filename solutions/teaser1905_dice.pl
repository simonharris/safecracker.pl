:- use_module(library(clpfd)).
:- use_module('../safe_cracker').


% Three friends each roll a dice* twice. Jackie didn't roll an even number, and
% her first throw was higher than Ethel's second. The total of Len's throws was
% higher than 3 and his first throw was lower than Ethel's second Jackie's
% second throw rolled higher than Len's second. If each roll was unique, what
% did each roll first and second?"

solution1905(Jackie1, Jackie2, Ethel1, Ethel2, Len1, Len2) :-

    % Three friends each roll a dice twice.
    Rolls = [Jackie1, Jackie2, Ethel1, Ethel2, Len1, Len2],
    Rolls ins 1..6,
    all_distinct(Rolls),

    % Jackie didn't roll an even number, and
    is_odd(Jackie1),
    is_odd(Jackie2),

    % Jackie's first throw was higher than Ethel's second.
    Jackie1 #> Ethel2,

    % The total of Len's throws was higher than 3
    Len1 + Len2 #> 3,

    % and his first throw was lower than Ethel's second.
    Len1 #< Ethel2,

    % Jackie's second throw rolled higher than Len's second.
    Jackie2 #> Len2,

    %If each roll was unique, what
    % did each roll first and second?"
    label(Rolls).


% Published solution:
% Jackie1 = 3, Jackie2 = 5, Ethel1 = 6, Ethel2 = 2, Len1 = 1, Len2 = 4