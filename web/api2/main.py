import asyncio
import json
from pathlib import Path
import shutil
import time

from fastapi import FastAPI, HTTPException, UploadFile
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import PlainTextResponse, StreamingResponse
from fastapi.staticfiles import StaticFiles

from config import EXAMPLE_DIR, UPLOAD_DIR
from lib import get_clues, get_count


app = FastAPI()

origins = [
    '*',
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=['*'],
    allow_headers=['*'],
)

app.mount('/static', StaticFiles(directory='static'), name='static')


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
    {
        'id': '20250629_6293',
        'name': '29th June, 2025',
    },
]

# utils -----------------------------------------------------------------------


def message(type, content):
    return json.dumps({
        'type': type,
        'content': content,
    })


async def get_clues_async(imgfile: str) -> list:
    return await asyncio.to_thread(get_clues, imgfile)


# routes ----------------------------------------------------------------------


@app.get('/examples')
def read_examples():
    return {'examples': EXAMPLES}


@app.post('/upload')
def upload(file: UploadFile):

    puzzle_file = Path(UPLOAD_DIR) / f"{file.filename}"

    with open(f"{puzzle_file}", 'wb') as f:
        shutil.copyfileobj(file.file, f)

    return PlainTextResponse(file.filename)


@app.get('/solve/example/{puzzle_id}')
async def read_solve_example(puzzle_id: str):
    puzzle_file = Path(EXAMPLE_DIR) / f"{puzzle_id}.jpg"
    return StreamingResponse(puzzle_stream(puzzle_file), media_type='text/event-stream')


@app.get('/solve/upload/{puzzle_filename}')
async def solve_upload(puzzle_filename: str):
    puzzle_file = Path(UPLOAD_DIR) / f"{puzzle_filename}"

    if not puzzle_file.exists():
        raise HTTPException(status_code=404, detail=f"File {puzzle_file} not found")

    return StreamingResponse(puzzle_stream(puzzle_file), media_type='text/event-stream')


# main processing -------------------------------------------------------------


async def puzzle_stream(puzzle_file):
    yield "event: begin\ndata: ...\n\n"
    yield f"event: message\ndata: { message('msg-phase', 'Awaiting puzzle...RECEIVED') }\n\n"
    yield f"event: message\ndata: { message('msg-phase', 'Beginning OCR') }\n\n"

    task = asyncio.create_task(get_clues_async(puzzle_file))

    ctr = 0
    while not task.done():
        msg = f"Beginning OCR{'.' * ctr}"
        yield f"event: update\ndata: { message('msg-phase', msg) }\n\n"
        await asyncio.sleep(1)
        ctr += 1

    clues = task.result()

    dots = '.' * ctr
    msg = f"Beginning OCR{dots}DONE"
    yield f"event: update\ndata: { message('msg-phase', msg) }\n\n"

    for clue in clues:
        yield f"event: message\ndata: { message('msg-clue', clue) }\n\n"

    yield f"event: message\ndata: { message('msg-phase', 'Consulting parser...DONE') }\n\n"
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
