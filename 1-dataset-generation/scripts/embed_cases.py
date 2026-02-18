#!/usr/bin/env python3
"""
Compute sentence embeddings for clustering analysis.

This script computes embeddings for test cases using sentence transformers,
which can be used clustering analysis.
"""

import argparse
import json
import logging
import os
import pickle
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from sentence_transformers import SentenceTransformer
from tqdm.auto import tqdm

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from utils.generation_utils import setup_logging


def load_jsonl_for_embedding(file_path):
    """Load JSONL file and extract fields needed for embedding computation."""
    data = []
    skipped_count = 0
    total_count = 0

    try:
        # First count lines for progress bar
        with open(file_path, "r") as f:
            total_lines = sum(1 for _ in f)

        with open(file_path, "r") as f:
            for line_num, line in enumerate(
                tqdm(f, total=total_lines, desc=f"Loading JSONL: {file_path.name}")
            ):
                try:
                    total_count += 1
                    item = json.loads(line.strip())

                    # Debug: Print first 5 items to see structure
                    if line_num < 5:
                        print(f"\nLine {line_num} structure:")
                        print("Keys in item:", list(item.keys()))
                        print("Sample key values for debugging:")
                        for key in list(item.keys())[
                            :3
                        ]:  # Print first few keys for debugging
                            try:
                                print(f"  {key}: {str(item[key])[:50]}...")
                            except:
                                print(f"  {key}: <error printing value>")

                    # Switch logic for dataset type
                    if "prompt_id" in item:  # processed_cases.jsonl
                        print(f"Line {line_num}: Matched processed_cases")
                        columns = [
                            "opening_prompt",
                            "target_conv_arr",
                            "antitarget_conv_arr",
                            "user_messages",
                            "target_messages",
                            "antitarget_messages",
                            "target_conv_str_wout_prompt",
                            "antitarget_conv_str_wout_prompt",
                            "target_string",
                            "antitarget_string",
                            "user_string",
                        ]
                        question_data = {
                            "row_id": item["prompt_id"],
                        }
                        for c in columns:
                            if c in item:
                                question_data[c] = item[c]
                            else:
                                print(f"Line {line_num}: Missing column: {c}")

                        data.append(question_data)

                    elif "choice_a" in item and "choice_b" in item:  # improved_data
                        print(f"Line {line_num}: Matched choice_a/b")
                        question_data = {
                            "row_id": line_num,
                            "question": item["question"],
                            "choice_a": item["choice_a"],
                            "choice_b": item["choice_b"],
                        }
                        if "question" in item and item["question"]:
                            data.append(question_data)
                        else:
                            skipped_count += 1
                            print(f"Line {line_num}: Skipped - missing question field")

                    else:  # lm_generated_evals
                        print(f"Line {line_num}: Matched default case")
                        if "question" in item:
                            question_data = {
                                "row_id": line_num,
                                "question": item["question"],
                            }
                            if item["question"]:
                                data.append(question_data)
                            else:
                                skipped_count += 1
                                print(f"Line {line_num}: Skipped - empty question")
                        else:
                            skipped_count += 1
                            print(
                                f"Line {line_num}: Skipped - missing question field, keys: {list(item.keys())}"
                            )

                except json.JSONDecodeError:
                    skipped_count += 1
                    print(f"\nError - Line {line_num + 1} is not valid JSON")
                    continue
                except KeyError as e:
                    skipped_count += 1
                    print(f"Line {line_num}: KeyError: {e}")
                    continue
                except Exception as e:
                    skipped_count += 1
                    print(f"Line {line_num}: Unexpected error: {e}")
                    continue

        # Print summary
        print("\nLoading summary:")
        print(f"Total entries processed: {total_count}")
        print(
            f"Entries skipped: {skipped_count} ({(skipped_count / total_count) * 100:.1f}%)"
        )
        print(f"Entries loaded: {len(data)}")

        # Verify we have data
        if len(data) == 0:
            print("WARNING: No entries were successfully loaded!")
            if skipped_count > 0:
                print(
                    "All entries were skipped due to errors. Check format of your JSONL file."
                )

        return data

    except Exception as e:
        logging.error(f"Failed to load file {file_path}: {e}")
        return None


def compute_embeddings(
    texts, model_name="sentence-transformers/paraphrase-mpnet-base-v2"
):
    """Compute embeddings using the specified sentence transformer model."""
    model = SentenceTransformer(model_name)
    embeddings = model.encode(
        texts,
        show_progress_bar=True,
        batch_size=32,
        device="cuda" if torch.cuda.is_available() else "cpu",
    )
    return embeddings


def process_file(file_path, output_dir):
    """Process a single JSONL file."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # Determine output file name
    if "processed_cases.jsonl" in str(file_path):
        output_file = output_dir / "relationship-seeking.pkl"
    else:
        dataset_type = "improved" if "improved_data" in str(file_path) else "orig"
        output_file = output_dir / f"{dataset_type}_{file_path.stem}.pkl"

    # Check if output file already exists
    if output_file.exists():
        logging.info(f"Output file already exists. Skipping: {output_file}")
        return

    logging.info(f"Processing file: {file_path}")

    print(file_path)
    # Load data
    data = load_jsonl_for_embedding(file_path)
    if data is None or len(data) == 0:
        logging.error(f"Skipping file due to errors or empty data: {file_path}")
        return

    df = pd.DataFrame(data)

    # Print columns for debugging
    print(f"DataFrame columns: {df.columns.tolist()}")
    print(f"DataFrame shape: {df.shape}")

    # Determine processing logic based on file path
    try:
        if "processed_cases.jsonl" in str(file_path):
            logging.info(
                "Detected processed_cases.jsonl - processing full question and choices."
            )

            #### FULL QUESTION ####
            df["full_question"] = df.apply(
                lambda x: f"{x['opening_prompt']} Choice A: {x['target_conv_str_wout_prompt']} Choice B: {x['antitarget_conv_str_wout_prompt']}",
                axis=1,
            )

            # Print sample for debugging
            print("\nSample 1 full_question:")
            print(df["full_question"].iloc[0] if not df.empty else "DataFrame is empty")

            print("\nSample 2 full_question:")
            print(
                df["full_question"].iloc[-1] if not df.empty else "DataFrame is empty"
            )

            df["embedding_full_question"] = list(
                compute_embeddings(df["full_question"].tolist())
            )

            ### FULL QUESTION STRINGS ###
            # Check if required columns exist
            str_columns = ["user_string", "target_string", "antitarget_string"]
            missing_str_columns = [col for col in str_columns if col not in df.columns]
            if missing_str_columns:
                print(f"WARNING: Missing string columns: {missing_str_columns}")
                # Fill missing columns with empty strings
                for col in missing_str_columns:
                    df[col] = ""

            df["full_question_str"] = df.apply(
                lambda x: f"{x['user_string']} Choice A: {x['target_string']} Choice B: {x['antitarget_string']}",
                axis=1,
            )

            # Print sample for debugging
            print("\nSample 1 full_question_str:")
            print(
                df["full_question_str"].iloc[0]
                if not df.empty
                else "DataFrame is empty"
            )

            print("\nSample 2 full_question_str:")
            print(
                df["full_question_str"].iloc[-1]
                if not df.empty
                else "DataFrame is empty"
            )

            df["embedding_full_question_str"] = list(
                compute_embeddings(df["full_question_str"].tolist())
            )

            ### FIRST USER-ASSISTANT TURN ONLY ####
            # Make a short question which is just first assistant turn
            df["first_user"] = df["user_messages"].apply(lambda x: x[0]["content"])

            df["target_first_assistant"] = df["target_conv_arr"].apply(
                lambda x: x[1]["content"]
            )
            df["antitarget_first_assistant"] = df["antitarget_conv_arr"].apply(
                lambda x: x[1]["content"]
            )

            df["short_question"] = df.apply(
                lambda x: f"{x['first_user']} Choice A: {x['target_first_assistant']} Choice B: {x['antitarget_first_assistant']}",
                axis=1,
            )

            # Print sample for debugging
            print("\nSample 1 short_question:")
            print(
                df["short_question"].iloc[0] if not df.empty else "DataFrame is empty"
            )

            print("\nSample 2 short_question:")
            print(
                df["short_question"].iloc[-1] if not df.empty else "DataFrame is empty"
            )

            df["embedding_short_question"] = list(
                compute_embeddings(df["short_question"].tolist())
            )

            #### CHOICE ASSISTANT OPTIONS ONLY ####

            # Print sample for debugging
            print("\nSample 1 target_string:")
            print(df["target_string"].iloc[0] if not df.empty else "DataFrame is empty")
            print("\nSample 1 antitarget_string:")
            print(
                df["antitarget_string"].iloc[0]
                if not df.empty
                else "DataFrame is empty"
            )

            print("\nSample 2 target_string:")
            print(
                df["target_string"].iloc[-1] if not df.empty else "DataFrame is empty"
            )
            print("\nSample 2 antitarget_string:")
            print(
                df["antitarget_string"].iloc[-1]
                if not df.empty
                else "DataFrame is empty"
            )

            # For consistency with the expected output structure
            df["embedding_full_choice_matching"] = list(
                compute_embeddings(df["target_string"].tolist())
            )

            df["embedding_full_choice_not_matching"] = list(
                compute_embeddings(df["antitarget_string"].tolist())
            )

            # Print sample for debugging
            print("\nSample 1 target_first_assistant:")
            print(
                df["target_first_assistant"].iloc[0]
                if not df.empty
                else "DataFrame is empty"
            )
            print("\nSample 1 antitarget_first_assistant:")
            print(
                df["antitarget_first_assistant"].iloc[0]
                if not df.empty
                else "DataFrame is empty"
            )

            print("\nSample 2 target_first_assistant:")
            print(
                df["target_first_assistant"].iloc[-1]
                if not df.empty
                else "DataFrame is empty"
            )
            print("\nSample 2 antitarget_first_assistant:")
            print(
                df["antitarget_first_assistant"].iloc[-1]
                if not df.empty
                else "DataFrame is empty"
            )

            df["embedding_choice_matching"] = list(
                compute_embeddings(df["target_first_assistant"].tolist())
            )

            df["embedding_choice_not_matching"] = list(
                compute_embeddings(df["antitarget_first_assistant"].tolist())
            )

        elif "improved_data" in str(file_path):
            logging.info("Detected improved_data - processing full question only.")
            df["full_question"] = df.apply(
                lambda x: f"{x['question']} Choice A: {x['choice_a']} Choice B: {x['choice_b']}",
                axis=1,
            )
            df["embedding_full_question"] = list(
                compute_embeddings(df["full_question"].tolist())
            )
            # Add empty columns for consistency
            df["embedding_short_question"] = None
            df["embedding_full_choice_matching"] = None
            df["embedding_full_choice_not_matching"] = None
            df["embedding_choice_matching"] = None
            df["embedding_choice_not_matching"] = None

        elif "lm_generated_evals" in str(file_path):
            logging.info("Detected lm_generated_evals - processing question only.")
            df["embedding_full_question"] = list(
                compute_embeddings(df["question"].tolist())
            )
            # Add empty columns for consistency
            df["embedding_short_question"] = None
            df["embedding_full_choice_matching"] = None
            df["embedding_full_choice_not_matching"] = None
            df["embedding_choice_matching"] = None
            df["embedding_choice_not_matching"] = None
        else:
            raise ValueError("Unrecognized file type or path.")

        # Save processed data
        result_df = df[
            [
                "row_id",
                "embedding_full_question",
                "embedding_short_question",
                "embedding_choice_matching",
                "embedding_choice_not_matching",
                "embedding_full_choice_matching",
                "embedding_full_choice_not_matching",
            ]
        ]
        with open(output_file, "wb") as f:
            pickle.dump(result_df, f)

        logging.info(f"Saved processed data to {output_file}")

    except Exception as e:
        logging.error(f"Error during processing of {file_path}: {e}")
        # Print traceback for debugging
        import traceback

        traceback.print_exc()


def main():
    parser = argparse.ArgumentParser(
        description="Compute sentence embeddings for clustering"
    )
    parser.add_argument(
        "--test",
        action="store_true",
        help="Test mode: process first file only, print results, no files written",
    )
    parser.add_argument(
        "--dataset",
        choices=["processed", "external", "all"],
        default="processed",
        help="Which dataset(s) to process: processed (our data), external (reference datasets), all",
    )
    parser.add_argument(
        "--output-dir",
        default="data/sentence_embeddings",
        help="Directory to save embeddings",
    )
    args = parser.parse_args()

    setup_logging()

    # Build list of input files based on dataset choice
    input_files = []

    if args.dataset in ["processed", "all"]:
        input_files.append(Path("data/processed_cases/processed_cases.jsonl"))

    if args.dataset in ["external", "all"]:
        # External reference datasets (must be cloned separately)
        external_files = [
            *Path("model-written-evals/improved_datasets").glob("*.jsonl"),
            *Path("evals/advanced-ai-risk/lm_generated_evals").glob("*.jsonl"),
        ]
        input_files.extend(external_files)

    if not input_files:
        logging.error("No input files found")
        return

    print(f"Processing {len(input_files)} files...")

    if args.test:
        print("\n=== TEST MODE ===")
        print(f"Would process: {[str(f) for f in input_files]}")
        print(f"Output dir: {args.output_dir}")

        # Actually compute one embedding to verify the pipeline works
        test_file = input_files[0]
        if not test_file.exists():
            print(f"Test file not found: {test_file}")
            return

        print(f"\nLoading first entry from {test_file}...")
        with open(test_file, "r") as f:
            first_line = f.readline().strip()
            first_item = json.loads(first_line)

        # Get a sample text to embed
        if "opening_prompt" in first_item:
            sample_text = first_item["opening_prompt"]
        elif "question" in first_item:
            sample_text = first_item["question"]
        else:
            sample_text = str(first_item)[:500]

        print(f"Sample text (truncated): {sample_text[:200]}...")
        print("\nComputing embedding...")

        embedding = compute_embeddings([sample_text])[0]
        print(f"Embedding shape: {embedding.shape}")
        print(f"Embedding sample (first 5 values): {embedding[:5]}")
        print("\nTest passed: embedding computation works.")
        return

    for file_path in input_files:
        process_file(file_path, args.output_dir)


if __name__ == "__main__":
    main()
