import json
import os
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from collections import defaultdict
from tqdm import tqdm

# Configuration
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
FOLDER_ROOT = os.path.dirname(SCRIPT_DIR)
REPO_ROOT = os.path.dirname(FOLDER_ROOT)

# Behavior to evaluate (change this to evaluate different behaviors)
BEHAVIOR = "relationship-seeking"

INPUT_FILE = os.path.join(FOLDER_ROOT, "data/model_responses.jsonl")
OUTPUT_FILE = os.path.join(FOLDER_ROOT, "data/scored_model_responses.jsonl")

# Add the path to import local modules
sys.path.append(SCRIPT_DIR)
from eval_rubrics import BEHAVIOR_RUBRIC
from utils.scoring import simple_conv_history_processer, score_response, append_record


def load_existing_scores():
    """Load existing scored records from JSONL file."""
    scored_pairs = set()
    if os.path.exists(OUTPUT_FILE):
        with open(OUTPUT_FILE, "r", encoding="utf-8") as f:
            for line in f:
                record = json.loads(line)
                pair = (record["entry_id"], record["model_id"])
                scored_pairs.add(pair)
    return scored_pairs


def main():
    # Load test prompts (for converting to proper format)
    test_prompts_file = os.path.join(REPO_ROOT, f"data/{BEHAVIOR}/test.jsonl")
    test_prompts = {}
    with open(test_prompts_file, "r") as f:
        for line in f:
            data = json.loads(line)
            test_prompts[data["entry_id"]] = data["prompt_arr"]

    # Load all model responses
    all_records = []
    with open(INPUT_FILE, "r", encoding="utf-8") as f:
        for line in f:
            all_records.append(json.loads(line))

    print(f"Loaded {len(all_records)} records from {INPUT_FILE}")

    # Load existing scored pairs
    scored_pairs = load_existing_scores()
    print(f"Found {len(scored_pairs)} already scored pairs")

    # Filter to records that need scoring
    to_score = []
    for record in all_records:
        pair = (record["entry_id"], record["model_id"])

        # Skip if already scored
        if pair in scored_pairs:
            continue

        # Skip if failed or empty response
        if record["status"] != "success":
            scored_record = record.copy()
            scored_record["relationship_score"] = None
            append_record(OUTPUT_FILE, scored_record)
            continue

        if not record.get("response") or record.get("response", "").strip() == "":
            scored_record = record.copy()
            scored_record["relationship_score"] = None
            append_record(OUTPUT_FILE, scored_record)
            continue

        to_score.append(record)

    print(f"Need to score: {len(to_score)} records")

    if not to_score:
        print("✓ All records already scored!")
        return

    MAX_WORKERS = 50

    # Group by entry for progress tracking
    records_by_entry = defaultdict(list)
    for record in to_score:
        records_by_entry[record["entry_id"]].append(record)

    print(f"\nScoring {len(to_score)} records across {len(records_by_entry)} entries")

    # Process records
    for entry_id in tqdm(sorted(records_by_entry.keys()), desc="Processing entries"):
        records = records_by_entry[entry_id]

        # Get prompt in proper format
        if entry_id in test_prompts:
            prompt_arr = test_prompts[entry_id]
            prompt_str = simple_conv_history_processer(prompt_arr)
        else:
            prompt_str = records[0]["prompt"]

        print(f"\nEntry {entry_id}: Scoring {len(records)} models")

        # Score in parallel
        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            future_to_record = {}

            for record in records:
                future = executor.submit(
                    score_response,
                    record["response"],
                    BEHAVIOR_RUBRIC,
                    prompt_str,
                )
                future_to_record[future] = record

            for future in tqdm(
                as_completed(future_to_record),
                total=len(records),
                desc=f"Entry {entry_id}",
                leave=False,
            ):
                record = future_to_record[future]
                score = future.result()

                scored_record = record.copy()
                scored_record["relationship_score"] = score

                append_record(OUTPUT_FILE, scored_record)

    # Final summary
    final_scored = load_existing_scores()
    print(f"\n✓ Scoring complete!")
    print(f"  Total scored records: {len(final_scored)}")
    print(f"  Saved to: {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
