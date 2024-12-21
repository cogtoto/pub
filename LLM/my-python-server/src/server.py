from flask import Flask, request, render_template, jsonify
import sympy as sp

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/calculate', methods=['POST'])
def calculate():
    data = request.get_json()
    formula = data.get('formula', '')
    try:
        result = sp.sympify(formula).evalf()
    except Exception as e:
        result = str(e)
    return jsonify({'result': str(result)})

if __name__ == '__main__':
    app.run(debug=True)