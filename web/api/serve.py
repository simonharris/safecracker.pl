import re
import time

import cv2
from flask import Flask, jsonify, Response
from flask_cors import CORS
import pytesseract


app = Flask(__name__)
CORS(app, send_wildcard=False)


DUMMY_CLUES = [
    "The fourth digit is greater than five",
    "The third digit is greater than six",
    "The second digit is greater than seven",
    "The first digit is greater than eight",
]

def get_clues(imgfile: str) -> list:
    return DUMMY_CLUES
    img = cv2.imread(imgfile)
    img = cv2.fastNlMeansDenoisingColored(img, None, 10, 10, 7, 15)
    img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    content = pytesseract.image_to_string(img)

    return re.findall(REGEX, content, re.MULTILINE)


# @app.route('/events')
# def events():
#     def event_stream():
#         ctr = 0
#         while True:
#             # Yield an event every now and then
#             yield f"data: Hello, world! {ctr}\n\n"
#             # Add a sleep or some other logic to control the event frequency
#             import time
#             time.sleep(2)  # Yield an event every 10 seconds
#             ctr += 1

#     return Response(event_stream(), mimetype="text/event-stream")


@app.route('/solve/<puzzle_id>')
def solve(puzzle_id):
    def puzzle_stream():

        yield "event: begin\ndata: Beginning OCR...\n\n"
        time.sleep(2)

        clues = get_clues(puzzle_id)

        for clue in clues:
            yield f"event: message\ndata: {clue}\n\n"

        yield "event: begin\ndata: Beginning OCR...\n\n"
        time.sleep(2)


        time.sleep(2)
        yield "event: end\ndata: finished\n\n"

    return Response(puzzle_stream(), mimetype="text/event-stream", status=200)


## main -----------------------------------------------------------------------


if __name__ == '__main__':
    app.run(debug=True)
