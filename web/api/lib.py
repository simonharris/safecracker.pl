import re
import subprocess

import cv2
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

def get_result(clues: list) -> dict:
    details = {}

    prolog_command = prolog_command = ["swipl", "-f", SOLVER_FILE, "-g", SOLVER_PRED]
    result = subprocess.run(prolog_command,
                            input='\n'.join(clues),
                            capture_output=True,
                            text=True)

    details['cmd'] = prolog_command
    details['output'] = result.stdout.strip()
    details['error'] = result.stderr.strip()
    return details


def get_count(clues: list) -> dict:
    details = {}

    prolog_command = prolog_command = ["swipl", "-f", SOLVER_FILE, "-g", COUNTER_PRED]
    result = subprocess.run(prolog_command,
                            input='\n'.join(clues),
                            capture_output=True,
                            text=True)
    lines = result.stdout.strip().split('\n')

    count = int(lines.pop(-1))
    final = lines.pop(-1)

    #details['cmd'] = prolog_command
    details['count'] = count
    details['final'] = final
    details['error'] = result.stderr.strip()
    return details
