import re
import sys

import cv2
import numpy as np
from PIL import Image
import pytesseract


REGEX = r'[1-5] ([A-Za-z-0-9 ]+)[\.\n]'

imgfile = sys.argv[1]


img = cv2.imread(imgfile)
img = cv2.fastNlMeansDenoisingColored(img, None, 10, 10, 7, 15)
img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
content = pytesseract.image_to_string(img)

#print(content)
matches = re.findall(REGEX, content, re.MULTILINE)

for match in matches:
    print(match)
