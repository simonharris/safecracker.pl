import re
import subprocess

import cv2
from pyswip import Prolog
import pytesseract

from config import COUNTER_PRED, SOLVER_FILE, SOLVER_PRED

OCR_REGEX = r'[1-5] ([A-Za-z-0-9 ]+)[\.\n]'

DUMMY_CLUES = [
    "The fourth digit is greater than five",
    "The third digit is greater than six",
    "The second digit is greater than seven",
    "The first digit is greater than eight",
]


def get_clues(imgfile: str) -> list:
    #return DUMMY_CLUES
    img = cv2.imread(imgfile)
    img = cv2.fastNlMeansDenoisingColored(img, None, 10, 10, 7, 15)
    img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    content = pytesseract.image_to_string(img)

    return re.findall(OCR_REGEX, content, re.MULTILINE)


def get_count(clues: list) -> dict:

    prolog = Prolog()
    prolog.consult(SOLVER_FILE)
    prolog.retractall("clue(_)")

    details = {}

    for clue in clues:
        prolog.asserta(f"clue('{clue}')")

    q = prolog.query("solution_count(A, B, C, D, Count)")
    sol = next(q)
    q.close()

    details['count'] = sol['Count']

    if sol['Count'] == 1:

        q = prolog.query("solution(A, B, C, D)")
        sol = next(q)
        q.close()

        details['final'] = list(sol.values())

    return details
