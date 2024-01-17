from flask import Flask, request, jsonify
from flask_cors import CORS
import connect4  # Assuming connect4 has relevant functions and isn't a Flask app itself
import digitrec  # Assuming digitrec contains the CNN model and digit recognition logic
import torch
from torchvision import transforms
from PIL import Image
import io
import base64
import os

app = Flask(__name__)
CORS(app, origins=["https://austinliu05.github.io", "http://localhost:3000"])

# Assuming connect4.py can be integrated here
@app.route('/connect4', methods=['POST'])
def play_connect4():
    # Extract data from request for connect4 game
    data = request.get_json()
    result = connect4.play_game(data)  # Replace with actual function call from connect4.py
    return jsonify(result), 200

# Route for digit recognition
@app.route('/recognize-digit', methods=['POST'])
def recognize_digit():
    # Assuming the image is sent as a base64 encoded string
    image_data = request.get_json()['image']
    image_name = request.get_json()['name']

    image = Image.open(io.BytesIO(base64.b64decode(image_data)))
    image = image.resize((28, 28))  # Resize the image to 28x28 pixels
    image = image.convert('L')  # Convert to grayscale
    
    # # Ensure the directory exists
    # save_directory = 'labeledIMG'
    # if not os.path.exists(save_directory):
    #     os.makedirs(save_directory)

    # # Save the image
    # image_path = os.path.join(save_directory, f'{image_name}.png')
    # image.save(image_path)
    
    # Transform the image (same as in digitrec.py)
    transform = transforms.Compose([
        transforms.Resize((28, 28)),
        transforms.ToTensor(),
        transforms.Normalize((0.1307,), (0.3081,))
    ])
    input_tensor = transform(image)
    data = input_tensor.unsqueeze(0)  # Add a batch dimension

    # Digit recognition (assuming digitrec.py contains a loaded model)
    output = digitrec.model(data)
    prediction = output.argmax(dim=1, keepdim=True).item()

    return jsonify({'prediction': prediction}), 200

if __name__ == '__main__':
    app.run(debug=False)
