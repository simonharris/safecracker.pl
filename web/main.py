from flask import Flask, render_template
from flask_bootstrap import Bootstrap5
from flask_wtf import FlaskForm
from wtforms import SubmitField

from lib import run_solve


## config ---------------------------------------------------------------------


class UploadForm(FlaskForm):
    submit = SubmitField(label='Solve')


## functions  -----------------------------------------------------------------




## app ------------------------------------------------------------------------


app = Flask(__name__)
Bootstrap5(app)
app.config['SECRET_KEY'] = '8BYkEfBA6O6donzWlSihBXox7C0sKR6b'

@app.route("/", methods=['GET', 'POST'])
def home():

    result = None
    form = UploadForm()

    if form.validate_on_submit():
        result = run_solve()

    return render_template('index.html', form=form, result=result)





## main -----------------------------------------------------------------------


if __name__ == '__main__':
    app.run(debug=True)
