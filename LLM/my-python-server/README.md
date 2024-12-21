# My Python Server

This project sets up a local HTTP server using Flask to evaluate mathematical formulas entered by the user. It serves an HTML page with a text box for input and displays the calculated results.

## Project Structure

```
my-python-server
├── src
│   ├── server.py          # Main script to launch the server
│   └── templates
│       └── index.html     # HTML file with input and output sections
├── requirements.txt        # Project dependencies
└── README.md               # Project documentation
```

## Requirements

To run this project, you need to have Python installed along with the required packages listed in `requirements.txt`.

## Installation

1. Clone the repository or download the project files.
2. Navigate to the project directory.
3. Install the required packages:

```
pip install -r requirements.txt
```

## Running the Server

To start the server, run the following command:

```
python src/server.py
```

The server will be accessible at `http://127.0.0.1:5000`.

## Usage

1. Open your web browser and go to `http://127.0.0.1:5000`.
2. Enter a mathematical formula in the text box.
3. The result will be displayed in the designated output section after submission.

## License

This project is open-source and available under the MIT License.