import shutil
from typing import Annotated

from fastapi import FastAPI, HTTPException, UploadFile
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import PlainTextResponse
from fastapi.staticfiles import StaticFiles

from config import UPLOAD_DIR


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


@app.get('/examples')
def read_examples():
    return {'examples': EXAMPLES}


@app.post('/upload')
def upload(file: UploadFile):

    with open(f"{UPLOAD_DIR}{file.filename}", 'wb') as f:
        shutil.copyfileobj(file.file, f)

    return PlainTextResponse(file.filename)
