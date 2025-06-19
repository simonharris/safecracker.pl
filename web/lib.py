import re
import subprocess

from PIL import Image
import pytesseract


INFILE = 'ocr/photo01.jpg'
REGEX = r'^[1-5] ([A-Za-z-0-9 ]+)[\.\n]'
SOLVER_FILE = 'solver.pl'
SOLVER_PRED = 'solution(A, B, C, D)'

DUMMY_CLUES = [
    "The fourth digit is greater than five",
    "The third digit is greater than six",
    "The second digit is greater than seven",
    "The first digit is greater than eight",
]

def get_clues(imgfile: str) -> list:
    return DUMMY_CLUES

    content = pytesseract.image_to_string(Image.open(imgfile))
    return re.findall(REGEX, content, re.MULTILINE)


def get_result(clues: list):
    details = {}

    prolog_command =prolog_command = ["swipl", "-f", SOLVER_FILE, "-g", SOLVER_PRED]
    result = subprocess.run(prolog_command,
                            input='\n'.join(clues),
                            capture_output=True,
                            text=True)

    details['cmd'] = prolog_command
    details['output'] = result.stdout.strip()
    details['error'] = result.stderr.strip()
    return details




def run_solve() -> str:

    output = {}

    clues = get_clues(INFILE)
    output['clues'] = clues
    output['result'] = get_result(clues)
    return output