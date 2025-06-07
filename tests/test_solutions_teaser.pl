:- begin_tests(brain_teaser_solutions).
:- consult('../solutions/teaser1904_graduation').


test(solution_1904_graduation) :-
    once(solution_1904(MathsVenue, HistoryVenue, LawVenue, MusicVenue)),
    assertion((MathsVenue = 8, HistoryVenue = 1, LawVenue = 4, MusicVenue = 2)).
