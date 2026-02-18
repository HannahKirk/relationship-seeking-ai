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

INPUT_FILE = os.path.join(REPO_ROOT, "2-steering-vector-training/vector_evals/Llama-3.1-70B-Instruct/layer31/generations_ep10.jsonl")
OUTPUT_FILE = os.path.join(FOLDER_ROOT, "data/scored_vector_generations_ep10.jsonl")
TEST_PROMPTS_FILE = os.path.join(REPO_ROOT, f"data/{BEHAVIOR}/test.jsonl")

# Target multipliers (lambda values)
TARGET_MULTIPLIERS = [-1.0, -0.5, 0.0, 0.5, 1.0]

# Number of test prompts to use (first N rows from test.jsonl)
NUM_TEST_PROMPTS = 100

# Add the path to import local modules
sys.path.append(SCRIPT_DIR)
from eval_rubrics import BEHAVIOR_RUBRIC
from utils.scoring import simple_conv_history_processer, score_response, append_record


def load_existing_scores():
    """Load existing scored records from JSONL file."""
    scored_keys = set()
    if os.path.exists(OUTPUT_FILE):
        with open(OUTPUT_FILE, "r", encoding="utf-8") as f:
            for line in f:
                record = json.loads(line)
                # Create unique key: test_prompt_id + multiplier + generation_id
                key = (
                    record["test_prompt_id"],
                    record["multiplier"],
                    record.get("generation_id", 0),
                )
                scored_keys.add(key)
    return scored_keys


def is_target_multiplier(multiplier):
    """Check if multiplier is in our target list (with tolerance for float comparison)."""
    for target in TARGET_MULTIPLIERS:
        if abs(multiplier - target) < 0.01:
            return True
    return False


def main():
    # Load first N rows from test.jsonl and get their prompt_ids
    # prompt_id in test.jsonl maps to test_prompt_id in generations file
    test_prompts = {}  # Maps prompt_id -> prompt_arr
    target_prompt_ids = set()

    print(f"Loading first {NUM_TEST_PROMPTS} rows from {TEST_PROMPTS_FILE}")
    with open(TEST_PROMPTS_FILE, "r") as f:
        for i, line in enumerate(f):
            if i >= NUM_TEST_PROMPTS:
                break
            data = json.loads(line)
            prompt_id = data["prompt_id"]  # This maps to test_prompt_id in generations
            target_prompt_ids.add(prompt_id)
            test_prompts[prompt_id] = data.get("prompt_arr", data.get("prompt"))

    print(
        f"Loaded {len(target_prompt_ids)} unique prompt_ids from first {NUM_TEST_PROMPTS} rows"
    )
    print(f"Sample prompt_ids: {sorted(target_prompt_ids)[:10]}... (showing first 10)")
    print(f"Prompt ID range: {min(target_prompt_ids)} to {max(target_prompt_ids)}")

    # Load all records from generations file
    all_records = []
    with open(INPUT_FILE, "r", encoding="utf-8") as f:
        for line in f:
            if line.strip():
                all_records.append(json.loads(line))

    print(f"Loaded {len(all_records)} total records from {INPUT_FILE}")

    # Filter records by multiplier and prompt_id (test_prompt_id in generations file)
    filtered_records = []
    for record in all_records:
        test_prompt_id = record.get(
            "test_prompt_id"
        )  # This is the prompt_id from test.jsonl
        multiplier = record.get("multiplier")

        # Check if test_prompt_id is in our target prompt_ids
        if test_prompt_id is None or test_prompt_id not in target_prompt_ids:
            continue

        # Check multiplier
        if multiplier is None or not is_target_multiplier(multiplier):
            continue

        filtered_records.append(record)

    print(
        f"Filtered to {len(filtered_records)} records with target multipliers {TARGET_MULTIPLIERS}"
    )
    print(
        f"Covering {len(set(r['test_prompt_id'] for r in filtered_records))} unique prompt_ids"
    )

    # Load existing scored records
    scored_keys = load_existing_scores()
    print(f"Found {len(scored_keys)} already scored records")

    # Filter to records that need scoring
    to_score = []
    for record in filtered_records:
        key = (
            record["test_prompt_id"],
            record["multiplier"],
            record.get("generation_id", 0),
        )

        if key in scored_keys:
            continue

        # Skip empty or gibberish responses (very high perplexity might indicate issues)
        response_str = record.get("response_str", "")
        if not response_str or response_str.strip() == "":
            scored_record = record.copy()
            scored_record["relationship_score"] = None
            scored_record["score_note"] = "empty_response"
            append_record(OUTPUT_FILE, scored_record)
            continue

        to_score.append(record)

    print(f"Need to score: {len(to_score)} records")

    if not to_score:
        print("✓ All target records already scored!")
        return

    # Group by test_prompt_id for progress tracking
    records_by_prompt = defaultdict(list)
    for record in to_score:
        records_by_prompt[record["test_prompt_id"]].append(record)

    print(
        f"\nScoring {len(to_score)} records across {len(records_by_prompt)} test prompts"
    )

    MAX_WORKERS = 20  # Conservative to avoid rate limits

    # Process records
    for test_prompt_id in tqdm(
        sorted(records_by_prompt.keys()), desc="Processing test prompts"
    ):
        records = records_by_prompt[test_prompt_id]

        # Get prompt context if available
        prompt_str = None
        if test_prompt_id in test_prompts:
            prompt_data = test_prompts[test_prompt_id]
            if isinstance(prompt_data, list):
                prompt_str = simple_conv_history_processer(prompt_data)
            else:
                prompt_str = str(prompt_data)

        # Score in parallel
        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            future_to_record = {}

            for record in records:
                future = executor.submit(
                    score_response,
                    record["response_str"],
                    BEHAVIOR_RUBRIC,
                    prompt_str,
                )
                future_to_record[future] = record

            for future in tqdm(
                as_completed(future_to_record),
                total=len(records),
                desc=f"Prompt {test_prompt_id}",
                leave=False,
            ):
                record = future_to_record[future]
                try:
                    score = future.result()
                except Exception as e:
                    print(f"Exception getting result: {e}")
                    score = None

                scored_record = record.copy()
                scored_record["relationship_score"] = score

                append_record(OUTPUT_FILE, scored_record)

    # Final summary
    final_scored = load_existing_scores()
    print(f"\n✓ Scoring complete!")
    print(f"  Total scored records: {len(final_scored)}")
    print(f"  Saved to: {OUTPUT_FILE}")

    # Print summary by multiplier
    print("\n  Records by multiplier:")
    multiplier_counts = defaultdict(int)
    if os.path.exists(OUTPUT_FILE):
        with open(OUTPUT_FILE, "r") as f:
            for line in f:
                record = json.loads(line)
                multiplier_counts[record["multiplier"]] += 1
    for mult in sorted(multiplier_counts.keys()):
        print(f"    λ={mult}: {multiplier_counts[mult]} records")


if __name__ == "__main__":
    main()
