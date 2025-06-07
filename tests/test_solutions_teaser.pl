:- begin_tests(brain_teaser_solutions).
:- consult('../solutions/teaser1904_graduation').
:- consult('../solutions/teaser1905_dice').

% These are largely pointless, but think of it as checking the solutions page


test(solution_1904_graduation) :-
    once(solution_1904(MathsVenue, HistoryVenue, LawVenue, MusicVenue)),
    assertion((MathsVenue = 8, HistoryVenue = 1, LawVenue = 4, MusicVenue = 2 )).

test(solution_1905_graduation) :-
    once(solution1905(Jackie1, Jackie2, Ethel1, Ethel2, Len1, Len2)),
    assertion((Jackie1 = 3, Jackie2 = 5, Ethel1 = 6, Ethel2 = 2, Len1 = 1, Len2 = 4)).
