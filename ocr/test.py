import re
import sys

from PIL import Image
import pytesseract


REGEX = r'^[1-5]\s([A-Za-z-0-9 ]+)[\.\n]'

imgfile = sys.argv[1]
content = pytesseract.image_to_string(Image.open(imgfile))
matches = re.findall(REGEX, content, re.MULTILINE)

for match in matches:
    print(match)
