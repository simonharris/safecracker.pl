from flask import Flask, render_template
from flask_bootstrap import Bootstrap5
from flask_wtf import FlaskForm
from werkzeug.utils import secure_filename
from wtforms import FileField, SubmitField
from wtforms.validators import DataRequired

from lib import run_solve


## config ---------------------------------------------------------------------


UPLOAD_DIR = 'web/static/uploads/'
#UPLOAD_DIR = '/tmp/'


## library -- -----------------------------------------------------------------


class UploadForm(FlaskForm):
    puzzle = FileField(label='Photo of puzzle', validators=[DataRequired(),])
    submit = SubmitField(label='Solve')



## app ------------------------------------------------------------------------


app = Flask(__name__)
Bootstrap5(app)
app.config['SECRET_KEY'] = '8BYkEfBA6O6donzWlSihBXox7C0sKR6b'

@app.route("/", methods=['GET', 'POST'])
def home():

    result = {}
    form = UploadForm()

    if form.validate_on_submit():
        filename = secure_filename(form.puzzle.data.filename)
        form.puzzle.data.save(UPLOAD_DIR + filename)
        result = run_solve(UPLOAD_DIR + filename)
        result['filename'] = filename
    return render_template('index.html', form=form, result=result)


## main -----------------------------------------------------------------------


if __name__ == '__main__':
    app.run(debug=True)
