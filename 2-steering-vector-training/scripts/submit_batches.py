"""Submit batch files to OpenAI Batch API."""

import argparse
import json
import os
import sys
from pathlib import Path

from openai import OpenAI

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from setup.api_utils import get_api_key
from exp_config import ModelConfig

COMPLETION_WINDOW = "24h"


def submit_batch(
    client: OpenAI,
    batch_file: Path,
    metadata_file: Path,
    description: str,
) -> dict:
    """Upload and submit a batch file to OpenAI.

    Returns the batch info dict.
    """
    # Upload the batch file
    with open(batch_file, "rb") as f:
        file_obj = client.files.create(file=f, purpose="batch")

    # Create the batch job
    batch_resp = client.batches.create(
        input_file_id=file_obj.id,
        endpoint="/v1/chat/completions",
        completion_window=COMPLETION_WINDOW,
        metadata={"description": description},
    )

    # Store the metadata
    batch_info = batch_resp.to_dict()
    with metadata_file.open("a") as mfile:
        mfile.write(json.dumps(batch_info) + "\n")

    return batch_info


def main():
    parser = argparse.ArgumentParser(description="Submit batch files to OpenAI")
    parser.add_argument(
        "--task",
        type=str,
        choices=["coherence", "relationship", "pairwise", "all"],
        default="all",
        help="Which task type to submit (default: all)",
    )
    parser.add_argument(
        "--epochs",
        type=int,
        nargs="+",
        default=[10, 15, 20],
        help="Epochs to process (default: 10 15 20)",
    )
    parser.add_argument(
        "--metadata_file",
        type=str,
        default="batched_ratings_metadata.jsonl",
        help="File to store batch metadata",
    )
    parser.add_argument(
        "--dry_run",
        action="store_true",
        help="Print what would be submitted without actually submitting",
    )
    args = parser.parse_args()

    client = OpenAI(api_key=get_api_key("OPENAI_API_KEY"))
    metadata_file_path = Path(args.metadata_file)

    tasks = (
        ["coherence", "relationship", "pairwise"]
        if args.task == "all"
        else [args.task]
    )

    submitted_count = 0

    for short_name, layer, _ in ModelConfig.get_experiments():
        base_dir = Path(f"vector_evals/{short_name}/layer{layer}")
        if not base_dir.exists():
            continue

        for epoch in args.epochs:
            for task in tasks:
                # Determine batch file name
                if task == "pairwise":
                    batch_file = base_dir / f"batch_pairwise_ep{epoch}.jsonl"
                else:
                    batch_file = base_dir / f"json_batch_{task}_ep{epoch}.jsonl"

                if not batch_file.exists():
                    continue

                description = f"{short_name} epoch={epoch} layer={layer} task={task}"

                if args.dry_run:
                    print(f"Would submit: {batch_file}")
                    print(f"  Description: {description}")
                else:
                    print(f"Submitting: {batch_file}")
                    batch_info = submit_batch(
                        client, batch_file, metadata_file_path, description
                    )
                    print(f"  Created batch: {batch_info['id']}")
                    submitted_count += 1

    if args.dry_run:
        print(f"\nDry run complete. Would have submitted batches.")
    else:
        print(f"\nSubmitted {submitted_count} batches.")
        print(f"Metadata saved to {metadata_file_path}")


if __name__ == "__main__":
    main()
