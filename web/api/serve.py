import os
import time

from flask import Flask, jsonify, request, Response
from flask_cors import CORS
from werkzeug.utils import secure_filename

from config import ALLOWED_EXTENSIONS, EXAMPLE_DIR, UPLOAD_DIR
from lib import get_count, get_clues


app = Flask(__name__)
CORS(app, send_wildcard=False)
app.config['UPLOAD_FOLDER'] = UPLOAD_DIR

EXAMPLES = [
    {
        'id': '20250511_9146',
        'name': '11th May, 2025',
    },
    {
        'id': '20250601_6452',
        'name': '1st June, 2025 (broken)',
    },
    {
        'id': '20250615_7846',
        'name': '15th June, 2025',
    },
]

## utils ----------------------------------------------------------------------


def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


## routes ---------------------------------------------------------------------


@app.route('/examples')
def examples():
    return jsonify(examples=EXAMPLES)


@app.route('/solve/example/<puzzle_id>')
def solve_example(puzzle_id):
    puzzle_file = EXAMPLE_DIR + puzzle_id + '.jpg'
    return solve(puzzle_file)


@app.route('/solve/upload/<puzzle_filename>')
def solve_upload(puzzle_filename):
    puzzle_file = UPLOAD_DIR + puzzle_filename
    return solve(puzzle_file)


def solve(puzzle_file):
    def puzzle_stream():

        yield "event: begin\ndata: Beginning OCR...\n\n"
        time.sleep(2)

        clues = get_clues(puzzle_file)

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


@app.route('/upload', methods=['POST'])
def upload():

    # check if the post request has the file part
    if 'file' not in request.files:
        pass
        # whatevs
    file = request.files['file']
    # If the user does not select a file, the browser submits an
    # empty file without a filename.
    if file.filename == '':
        pass
    if file and allowed_file(file.filename):
        filename = secure_filename(file.filename)
        file.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))

        return Response(filename, status=200)


## main -----------------------------------------------------------------------


if __name__ == '__main__':
    app.run(debug=True)
