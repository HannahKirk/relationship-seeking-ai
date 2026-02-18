import requests
import json
from tqdm import tqdm
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading
import os
import sys

# Configuration
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
FOLDER_ROOT = os.path.dirname(SCRIPT_DIR)
REPO_ROOT = os.path.dirname(FOLDER_ROOT)

# Behavior to evaluate (change this to evaluate different behaviors)
BEHAVIOR = "relationship-seeking"

TEST_FILE = os.path.join(REPO_ROOT, f"data/{BEHAVIOR}/test.jsonl")
OUTPUT_FILE = os.path.join(FOLDER_ROOT, "data/model_responses.jsonl")

# Import MODELS_TO_INCLUDE from candidate_models.py
sys.path.append(SCRIPT_DIR)
from candidate_models import MODELS_TO_INCLUDE

# OpenRouter API key from environment variable
OPENROUTER_API_KEY = os.environ.get("OPENROUTER_API_KEY")
if not OPENROUTER_API_KEY:
    raise ValueError("OPENROUTER_API_KEY environment variable not set")

# Thread-safe file writing
write_lock = threading.Lock()


def query_model(model_name, model_id, prompt, entry_id):
    """Query a single model and return the result."""
    try:
        response = requests.post(
            url="https://openrouter.ai/api/v1/chat/completions",
            headers={
                "Authorization": f"Bearer {OPENROUTER_API_KEY}",
                "Content-Type": "application/json",
            },
            json={
                "model": model_id,
                "messages": [{"role": "user", "content": prompt}],
                "thinking": {"type": "disabled", "budget_tokens": 0},
            },
            timeout=120,
        )

        if response.status_code == 200:
            response_data = response.json()
            return {
                "entry_id": entry_id,
                "prompt": prompt,
                "model_name": model_name,
                "model_id": model_id,
                "response": response_data["choices"][0]["message"]["content"],
                "error": None,
                "status": "success",
            }
        else:
            return {
                "entry_id": entry_id,
                "prompt": prompt,
                "model_name": model_name,
                "model_id": model_id,
                "response": None,
                "error": f"Status code: {response.status_code}",
                "status": "failed",
            }

    except Exception as e:
        return {
            "entry_id": entry_id,
            "prompt": prompt,
            "model_name": model_name,
            "model_id": model_id,
            "response": None,
            "error": str(e)[:200],
            "status": "failed",
        }


def load_existing_pairs():
    """Load existing model-entry pairs from JSONL file."""
    existing_pairs = set()
    if os.path.exists(OUTPUT_FILE):
        with open(OUTPUT_FILE, "r", encoding="utf-8") as f:
            for line in f:
                record = json.loads(line)
                pair = (record["entry_id"], record["model_id"])
                existing_pairs.add(pair)
    return existing_pairs


def append_record(record):
    """Append a single record to the JSONL file."""
    with write_lock:
        with open(OUTPUT_FILE, "a", encoding="utf-8") as f:
            f.write(json.dumps(record, ensure_ascii=False) + "\n")


# Load existing pairs
existing_pairs = load_existing_pairs()
print(f"Loaded {len(existing_pairs)} existing model-entry pairs from {OUTPUT_FILE}")

# Fetch all available models
print("\nFetching all available models from OpenRouter...")
response = requests.get("https://openrouter.ai/api/v1/models")

if response.status_code != 200:
    print(f"Failed to fetch models: {response.status_code}")
    exit(1)

models_data = response.json()

# Create model list - only include models in MODELS_TO_INCLUDE
model_list = {}
found_models = set()

for model in models_data["data"]:
    model_id = model["id"]

    # Check if this model is in our include list
    if model_id in MODELS_TO_INCLUDE:
        pricing = model.get("pricing", {})

        if pricing:
            try:
                prompt_cost = float(pricing.get("prompt", "0"))
                completion_cost = float(pricing.get("completion", "0"))
                if prompt_cost >= 0 and completion_cost >= 0:
                    key = model_id.replace("/", "_").replace("-", "_").replace(":", "_")
                    model_list[key] = model_id
                    found_models.add(model_id)
            except:
                pass

# Check which models from our list are missing
missing_models = MODELS_TO_INCLUDE - found_models

print(f"\n✓ Found {len(model_list)} models from the curated list")
print(f"  Total models in curated list: {len(MODELS_TO_INCLUDE)}")

if missing_models:
    print(
        f"\n⚠️  Warning: {len(missing_models)} models from curated list not found on OpenRouter:"
    )
    for model_id in sorted(missing_models)[:10]:
        print(f"  - {model_id}")
    if len(missing_models) > 10:
        print(f"  ... and {len(missing_models) - 10} more")

# Load test prompts
test_prompts = []
with open(TEST_FILE, "r", encoding="utf-8") as f:
    for line in f:
        data = json.loads(line)
        first_user_message = data["prompt_arr"][0]["content"]
        test_prompts.append(
            {
                "entry_id": data["entry_id"],
                "prompt": first_user_message,
                "full_data": data,
            }
        )

print(f"Loaded {len(test_prompts)} test prompts")

# # Sample 50 prompts
# test_prompts = test_prompts[:50]
# Sample
# Sample 50 prompts
# test_prompts = test_prompts[50:100]
test_prompts = test_prompts[0:100]

# Calculate what needs to be queried
total_pairs = len(test_prompts) * len(model_list)
already_done = 0
to_query = []

for test_item in test_prompts:
    entry_id = test_item["entry_id"]
    prompt = test_item["prompt"]

    for model_name, model_id in model_list.items():
        pair = (entry_id, model_id)
        if pair in existing_pairs:
            already_done += 1
        else:
            to_query.append(
                {
                    "entry_id": entry_id,
                    "prompt": prompt,
                    "model_name": model_name,
                    "model_id": model_id,
                }
            )

print(f"\n{'='*60}")
print(f"Total possible pairs: {total_pairs}")
print(f"Already completed: {already_done} ({100*already_done/total_pairs:.1f}%)")
print(f"Need to query: {len(to_query)} ({100*len(to_query)/total_pairs:.1f}%)")
print(f"{'='*60}")

if not to_query:
    print("\n✓ All model-entry pairs already processed!")
    exit(0)

# Number of parallel workers
MAX_WORKERS = 103

print(f"\nStarting data collection with {MAX_WORKERS} parallel workers...\n")

# Group queries by entry for better progress tracking
from collections import defaultdict

queries_by_entry = defaultdict(list)
for q in to_query:
    queries_by_entry[q["entry_id"]].append(q)

# Process all queries
for entry_id in sorted(queries_by_entry.keys()):
    queries = queries_by_entry[entry_id]

    print(f"\n{'='*60}")
    print(f"Processing Entry ID: {entry_id}")
    print(f"Querying {len(queries)} models for this entry")
    print(f"{'='*60}")

    # Use ThreadPoolExecutor for parallel requests
    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        # Submit all tasks for this entry
        future_to_query = {
            executor.submit(
                query_model, q["model_name"], q["model_id"], q["prompt"], q["entry_id"]
            ): q
            for q in queries
        }

        # Process completed tasks with progress bar
        for future in tqdm(
            as_completed(future_to_query),
            total=len(queries),
            desc=f"Entry {entry_id}",
        ):
            result = future.result()

            # Append result to JSONL file immediately
            append_record(result)

    print(f"✓ Completed entry {entry_id}")

print("\n" + "=" * 60)
print("DATA COLLECTION COMPLETE")
print("=" * 60)
print(f"All responses saved to {OUTPUT_FILE}")

# Count final statistics
final_pairs = load_existing_pairs()
successful = 0
failed = 0

if os.path.exists(OUTPUT_FILE):
    with open(OUTPUT_FILE, "r", encoding="utf-8") as f:
        for line in f:
            record = json.loads(line)
            if record["status"] == "success":
                successful += 1
            else:
                failed += 1

print(f"\nTotal pairs in file: {len(final_pairs)}")
print(f"Successful: {successful} ({100*successful/(successful+failed):.1f}%)")
print(f"Failed: {failed} ({100*failed/(successful+failed):.1f}%)")
print("\n" + "=" * 60)
