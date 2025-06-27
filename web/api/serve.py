import asyncio
import json
import os
import time

from flask import (
    Flask, jsonify, request, Response, stream_with_context
)
from flask_cors import CORS
from werkzeug.utils import secure_filename

from config import ALLOWED_EXTENSIONS, EXAMPLE_DIR, UPLOAD_DIR
from lib import get_count, get_clues, get_clues_async


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


def message(type, content):
    return json.dumps({
        'type': type,
        'content': content,
    })


def puzzle_stream(puzzle_file):
    async def fetch_clues():
        clues = await get_clues_async(puzzle_file)
        return clues

    yield "event: begin\ndata: ...\n\n"
    yield f"event: message\ndata: { message('msg-phase', 'Awaiting puzzle...RECEIVED') }\n\n"
    yield f"event: message\ndata: { message('msg-phase', 'Beginning OCR') }\n\n"

    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    task = loop.create_task(fetch_clues())

    ctr = 0
    while not task.done():
        msg = f"Beginning OCR{'.' * ctr}"
        yield f"event: update\ndata: { message('msg-phase', msg) }\n\n"
        loop.run_until_complete(asyncio.sleep(1))
        ctr += 1

    clues = task.result()

    dots = '.' * ctr
    msg = f"Beginning OCR{dots}DONE"
    yield f"event: update\ndata: { message('msg-phase', msg) }\n\n"

    for clue in clues:
        yield f"event: message\ndata: { message('msg-clue', clue) }\n\n"

    yield f"event: message\ndata: { message('msg-phase', 'Consulting parser...') }\n\n"
    time.sleep(1)
    yield f"event: update\ndata: { message('msg-phase', 'Consulting parser...DONE') }\n\n"
    yield f"event: message\ndata: { message('msg-phase', 'Applying constraints...') }\n\n"

    constraints = []
    results = get_count(constraints)
    msg = f"Candidate solutions remaining: { results['count'] }"
    yield f"event: message\ndata: { message('msg-progress', msg) }\n\n"

    for clue in clues:
        constraints.append(clue)

        msg = f"Applying '{clue}'"
        yield f"event: message\ndata: { message('msg-clue', msg) }\n\n"
        results = get_count(constraints)
        msg = f"Candidate solutions remaining: { results['count'] }"
        yield f"event: message\ndata: { message('msg-progress', msg) }\n\n"

    if results['count'] == 1:
        yield f"event: message\ndata: { message('msg-phase', 'Retrieving solution...') }\n\n"
        time.sleep(1)
        yield f"event: update\ndata: { message('msg-phase', 'Retrieving solution...DONE') }\n\n"
        msg = f"Solution: { results['final'] }"
        yield f"event: message\ndata: { message('msg-solution', msg) }\n\n"

    yield f"event: message\ndata: { message('msg-phase', 'Finished.') }\n\n"
    yield "event: end\ndata: finished\n\n"


def solve(puzzle_file):
    return Response(puzzle_stream(puzzle_file), mimetype="text/event-stream", status=200)


## routes ---------------------------------------------------------------------


@app.route('/examples')
def examples():
    return jsonify(examples=EXAMPLES)


@app.route('/solve/example/<puzzle_id>')
def solve_example(puzzle_id):
    puzzle_file = EXAMPLE_DIR + puzzle_id + '.jpg'
    return solve(puzzle_file)


@app.route('/solve/upload/<puzzle_filename>')
async def solve_upload(puzzle_filename):
    puzzle_file = UPLOAD_DIR + puzzle_filename
    return await solve(puzzle_file)


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
    app.run(debug=True, host='0.0.0.0', port=80)
