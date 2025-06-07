:- use_module(library(clpfd)).


% facts -----------------------------------------------------------------------


% you don't need this, except as documentation
venue_id(manorhall, 1).
venue_id(cathedral, 2).
venue_id(garden, 4).
venue_id(courtyard, 8).


% rules -----------------------------------------------------------------------


solution_1904(MathsVenue, HistoryVenue, LawVenue, MusicVenue) :-
    Venues = [MathsVenue, HistoryVenue, LawVenue, MusicVenue],
    Venues ins 1 \/ 2 \/ 4 \/ 8,
    all_distinct(Venues),

    LawVenue #\= 1,
    LawVenue #\= 2,

    (MusicVenue #= 1; MusicVenue #= 2),
    (HistoryVenue #= 1; HistoryVenue #= 8),

    HistoryVenue #= 8 #<==> MathsVenue #= 4,

    label(Venues).


% Published solution:
% Law - garden; History - manorhall; Music - cathedral; Maths - courtyard

% findall([MathsVenue, HistoryVenue, LawVenue, MusicVenue], solution(MathsVenue, HistoryVenue, LawVenue, MusicVenue), Solutions), length(Solutions, Count).