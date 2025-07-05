from fastapi import FastAPI

app = FastAPI()

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


@app.get("/examples")
def read_examples():
    return {'examples': EXAMPLES}
