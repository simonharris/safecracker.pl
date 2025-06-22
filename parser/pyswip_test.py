from pprint import pprint

from pyswip import Prolog


DUMMY_CLUES = [
    "The fourth digit is greater than five",
    "The third digit is greater than six",
    "The second digit is greater than seven",
    #"The first digit is greater than eight",
]

prolog = Prolog()
prolog.consult('parser/solver.pl')

prolog.retractall("clue(_)")

for clue in DUMMY_CLUES:
    prolog.asserta(f"clue('{clue}')")


q = prolog.query("solution_count(A, B, C, D, Count)")
for sol in q:
    print(sol)
q.close()



