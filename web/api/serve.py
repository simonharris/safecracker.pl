import time

from flask import Flask, jsonify, Response
from flask_cors import CORS

from lib import get_count, get_clues


app = Flask(__name__)
CORS(app, send_wildcard=False)


#INFILE = 'web/api/static/examples/20250511_9146.jpg'
#INFILE = 'web/api/static/examples/20250601_6452.jpg'
INFILE = 'web/api/static/examples/20250615_7846.jpg'


EXAMPLES = {
    '20250511_9146': '11th May, 2025',
    '20250601_6452': '1st June, 2025',
    '20250615_7846': '15th June, 2025 (broken)',
}


@app.route('/examples')
def examples():
    return jsonify(examples=EXAMPLES)


@app.route('/solve/<puzzle_id>')
def solve(puzzle_id):
    def puzzle_stream():

        yield "event: begin\ndata: Beginning OCR...\n\n"
        time.sleep(2)

        clues = get_clues(INFILE)

        for clue in clues:
            yield f"event: message\ndata: {clue}\n\n"

        yield "event: message\ndata: Consulting parser...\n\n"
        time.sleep(2)
        yield "event: message\ndata: Applying constraints...\n\n"

        constraints = []
        for clue in clues:
            constraints.append(clue)

            yield f"event: message\ndata: Applying {clue}...\n\n"
            results = get_count(constraints)
            yield f"event: message\ndata: {results}\n\n"

        time.sleep(2)
        yield "event: end\ndata: finished\n\n"

    return Response(puzzle_stream(), mimetype="text/event-stream", status=200)


## main -----------------------------------------------------------------------


if __name__ == '__main__':
    app.run(debug=True)
