"""Download completed batch results from OpenAI Batch API."""

import argparse
import json
import os
import sys
from pathlib import Path

from openai import OpenAI

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from setup.api_utils import get_api_key


def main():
    parser = argparse.ArgumentParser(description="Download completed batch results")
    parser.add_argument(
        "--metadata_file",
        type=str,
        default="batched_ratings_metadata.jsonl",
        help="File containing batch metadata",
    )
    parser.add_argument(
        "--dry_run",
        action="store_true",
        help="Print status without downloading",
    )
    parser.add_argument(
        "--task-name",
        type=str,
        default="relationship",
        help="Task name for pairwise output filename (default: relationship)",
    )
    args = parser.parse_args()
    task_name = args.task_name.replace("-", "_")  # Normalize for filename

    metadata_file_path = Path(args.metadata_file)
    if not metadata_file_path.exists():
        print(f"Metadata file not found: {metadata_file_path}")
        return

    # Load batch metadata
    all_batch_ids = []
    batch_metadata = []
    with open(metadata_file_path, "r") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            data = json.loads(line)
            all_batch_ids.append(data["id"])
            batch_metadata.append(data)

    print(f"Found {len(all_batch_ids)} total batch IDs in {metadata_file_path}.")

    # Initialize OpenAI client
    client = OpenAI(api_key=get_api_key("OPENAI_API_KEY"))

    # Check status and download completed batches
    completed_count = 0
    downloaded_count = 0
    pending_count = 0

    for batch_data in batch_metadata:
        batch_id = batch_data["id"]
        batch_obj = client.batches.retrieve(batch_id)

        if batch_obj.status == "completed":
            completed_count += 1

            # Extract metadata from description
            description = batch_data.get("metadata", {}).get("description", "")
            if not description:
                print(f"Warning: No description for batch {batch_id}")
                continue

            # Parse description: "Llama-3.1-70B-Instruct epoch=10 layer=21 task=coherence"
            # or older format: "Llama-3.1-70B-Instruct epoch=10 layer=21 mode=json"
            parts = description.split(" ")
            model = parts[0]
            epoch = parts[1].split("=")[-1]
            layer = parts[2].split("=")[-1]

            # Get task from description or from input filename
            task = None
            if len(parts) > 3 and "task=" in parts[3]:
                task = parts[3].split("=")[-1]
            else:
                # Fall back to parsing input filename
                input_file_id = batch_data.get("input_file_id")
                if input_file_id:
                    file_obj = client.files.retrieve(input_file_id)
                    filename = file_obj.filename
                    # Parse task from filename like "batch_pairwise_ep20.jsonl"
                    # or "json_batch_relationship_ep20.jsonl"
                    if "pairwise" in filename:
                        task = "pairwise"
                    elif "coherence" in filename:
                        task = "coherence"
                    elif "relationship" in filename:
                        task = "relationship"

            if not task:
                print(f"Warning: Could not determine task for batch {batch_id}")
                continue

            # Determine output path
            save_folder = Path(f"vector_evals/{model}/layer{layer}")
            save_folder.mkdir(parents=True, exist_ok=True)

            if task == "pairwise":
                save_filename = f"raw_pairwise_{task_name}_scores_ep{epoch}.jsonl"
            else:
                save_filename = f"raw_{task}_scores_ep{epoch}.jsonl"

            output_path = save_folder / save_filename

            if args.dry_run:
                print(f"Would download: {batch_id} -> {output_path}")
                continue

            # Check if already downloaded
            if output_path.exists():
                print(f"Already exists: {output_path}")
                continue

            # Download the results
            output_file_id = batch_obj.output_file_id
            if not output_file_id:
                print(f"No output file for batch {batch_id}")
                continue

            file_resp = client.files.content(output_file_id)
            result_text = file_resp.text

            with open(output_path, "w", encoding="utf-8") as f:
                f.write(result_text)

            print(f"Downloaded: {output_path}")
            downloaded_count += 1

        elif batch_obj.status in ["in_progress", "validating", "finalizing"]:
            print(f"Batch {batch_id}: {batch_obj.status}")
            pending_count += 1

        elif batch_obj.status == "failed":
            print(f"Batch {batch_id}: FAILED")
            if batch_obj.error_file_id:
                error_content = client.files.content(batch_obj.error_file_id)
                print(f"  Error: {error_content.text[:500]}")

        else:
            print(f"Batch {batch_id}: {batch_obj.status}")

    print(f"\nSummary:")
    print(f"  Completed: {completed_count}")
    print(f"  Downloaded: {downloaded_count}")
    print(f"  Pending: {pending_count}")


if __name__ == "__main__":
    main()
