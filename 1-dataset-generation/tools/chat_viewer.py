import json
import os
import random

from flask import Flask, jsonify, render_template, request

app = Flask(__name__)

# Get parent directory for relative paths
PARENT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


# Load the data
def load_data():
    conversations = []
    data_path = os.path.join(PARENT_DIR, "data/processed_cases/processed_cases.jsonl")
    with open(data_path, "r") as f:
        for line in f:
            conversations.append(json.loads(line))
    return conversations


def filter_conversations(conversations, filters):
    filtered = conversations.copy()
    print(f"Initial conversations: {len(filtered)}")

    # Debug print the incoming filters
    print(f"Raw filters received: {filters}")

    # Extract values safely from the filters dictionary
    n_turns = str(filters.get("n_turns", "")) if filters.get("n_turns") else ""
    model = str(filters.get("model", "")) if filters.get("model") else ""

    print(f"Processed filter values: n_turns={n_turns}, model={model}")

    # Only apply filter if value is not empty
    if n_turns:
        try:
            n_turns = int(n_turns)
            filtered = [c for c in filtered if c.get("n_turns") == n_turns]
            print(f"After n_turns filter ({n_turns}): {len(filtered)}")
        except (ValueError, TypeError) as e:
            print(f"Error processing n_turns filter: {e}")

    if model:
        filtered = [c for c in filtered if c.get("model") == model]
        print(f"After model filter ({model}): {len(filtered)}")

    return filtered


@app.route("/random-conversation", methods=["POST"])
def random_conversation():
    filters = request.get_json()
    print(f"Received filters: {filters}")

    conversations = load_data()
    print(f"Total conversations loaded: {len(conversations)}")

    filtered_conversations = filter_conversations(conversations, filters)
    print(f"Filtered conversations: {len(filtered_conversations)}")

    if not filtered_conversations:
        return jsonify({"error": "No conversations match the criteria"}), 404

    selected = random.choice(filtered_conversations)

    # Extract the necessary data for display
    result = {
        "prompt_id": selected.get("prompt_id"),
        "model": selected.get("model"),
        "n_turns": selected.get("n_turns"),
        "target_conversation": selected.get("target_conv_arr", []),
        "antitarget_conversation": selected.get("antitarget_conv_arr", [])
    }

    print(f"Selected conversation - prompt_id: {selected.get('prompt_id')}")
    return jsonify(result)


@app.route("/get-models", methods=["GET"])
def get_models():
    conversations = load_data()
    # Extract unique models from the dataset
    models = list(set(c.get("model", "") for c in conversations if c.get("model")))
    return jsonify({"models": models})


@app.route("/")
def index():
    return render_template("index.html")


if __name__ == "__main__":
    app.run(debug=True)
