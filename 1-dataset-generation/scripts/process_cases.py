#!/usr/bin/env python3
"""
Process parsed test cases into final format.

This script:
1. Loads and concatenates parsed cases (programmatic + LLM-fixed)
2. Filters and validates conversations
3. Explodes into individual prompts
4. Formats conversations into various representations
5. Optionally tokenizes and adds token counts
6. Saves processed cases
"""

import argparse
import json
import os
import sys

import pandas as pd
from tqdm import tqdm

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from utils.generation_utils import setup_logging
from utils.conversation_template import get_conv_template


def load_and_concatenate(input_dir):
    """Load parsed cases and LLM-fixed cases, concatenate them."""
    parsed_path = os.path.join(input_dir, "parsed_cases.jsonl")
    llm_fixed_path = os.path.join(input_dir, "parsed_cases_llm_fixed.jsonl")

    frames = []

    if os.path.exists(parsed_path):
        df_parsed = pd.read_json(parsed_path, lines=True)
        print(f"Loaded {len(df_parsed)} programmatically parsed cases")
        frames.append(df_parsed)
    else:
        print(f"Warning: {parsed_path} not found")

    if os.path.exists(llm_fixed_path):
        df_fixed = pd.read_json(llm_fixed_path, lines=True)
        print(f"Loaded {len(df_fixed)} LLM-fixed cases")
        frames.append(df_fixed)
    else:
        print(f"Warning: {llm_fixed_path} not found")

    if not frames:
        raise FileNotFoundError(f"No parsed case files found in {input_dir}")

    df = pd.concat(frames, ignore_index=True)
    return df


def filter_valid_cases(df):
    """Filter to only valid, successfully parsed cases."""
    initial_count = len(df)

    # Filter by parsing success
    df = df[df["parsing_success"] == True].copy()
    print(f"After parsing_success filter: {len(df)}/{initial_count}")

    # Filter out empty conversations
    df = df[df["conversations"].apply(lambda x: x != {} and x != [] and x is not None)]
    print(f"After empty conversations filter: {len(df)}")

    return df


def check_conversation(conversation):
    """Validate conversation structure."""
    if not isinstance(conversation, list):
        return False

    for turn in conversation:
        if not isinstance(turn, dict):
            return False
        if (
            "user_message" not in turn
            or "target_response" not in turn
            or "antitarget_response" not in turn
        ):
            return False
        if (
            not turn["user_message"]
            or not turn["target_response"]
            or not turn["antitarget_response"]
        ):
            return False

    return True


def explode_and_format(df):
    """Explode conversations and format into individual prompts."""
    # Count testcases per metaprompt
    df["n_testcases"] = df["conversations"].apply(len)

    print(f"Number of metaprompts: {len(df)}")
    print(f"Number of testcases: {df['n_testcases'].sum()}")

    if "total_cost" in df.columns:
        print(f"Total cost: ${df['total_cost'].sum():.2f}")

    # Explode into separate rows
    df = df.explode("conversations")

    # Keep only relevant columns
    keep_cols = ["metaprompt_id", "model", "prompt_type", "conversations"]
    available_cols = [c for c in keep_cols if c in df.columns]
    df = df[available_cols].copy()

    # Reset index and create prompt_id
    df.reset_index(drop=True, inplace=True)
    df["prompt_id"] = df.index

    # Expand conversations dict into columns
    df = pd.concat(
        [
            df.drop(["conversations"], axis=1),
            df["conversations"].apply(pd.Series),
        ],
        axis=1,
    )

    # Validate conversations
    df["valid_conversation"] = df["conversation"].apply(check_conversation)
    print(f"\nConversation validity:")
    print(df[["model", "valid_conversation"]].value_counts())

    # Filter to valid only
    invalid_count = (~df["valid_conversation"]).sum()
    if invalid_count > 0:
        print(f"\nDropping {invalid_count} invalid conversations")
    df = df[df["valid_conversation"] == True].copy()
    df.drop("valid_conversation", axis=1, inplace=True)

    # Reset index and prompt_id to be contiguous after filtering
    df.reset_index(drop=True, inplace=True)
    df["prompt_id"] = df.index

    # Add n_turns
    df["n_turns"] = df["conversation"].apply(len)

    return df


def construct_conversation_formats(df):
    """Construct various conversation format representations."""

    def construct_formats(row):
        conversation = row["conversation"]
        user_messages = []
        target_messages = []
        antitarget_messages = []
        target_convo = []
        antitarget_convo = []

        for turn in conversation:
            user_msg = {"role": "user", "content": turn["user_message"]}
            target_msg = {"role": "assistant", "content": turn["target_response"]}
            antitarget_msg = {"role": "assistant", "content": turn["antitarget_response"]}

            user_messages.append(user_msg)
            target_messages.append(target_msg)
            antitarget_messages.append(antitarget_msg)

            target_convo.append(user_msg)
            target_convo.append(target_msg)
            antitarget_convo.append(user_msg)
            antitarget_convo.append(antitarget_msg)

        return (
            user_messages,
            target_messages,
            antitarget_messages,
            target_convo,
            antitarget_convo,
        )

    print("Constructing conversation formats...")
    tqdm.pandas(desc="Processing")

    results = df.progress_apply(construct_formats, axis=1)
    (
        df["user_messages"],
        df["target_messages"],
        df["antitarget_messages"],
        df["target_conv_arr"],
        df["antitarget_conv_arr"],
    ) = zip(*results)

    # Create string representations
    df["user_string"] = df["user_messages"].apply(
        lambda x: " ".join([y["content"] for y in x])
    )
    df["target_string"] = df["target_messages"].apply(
        lambda x: " ".join([y["content"] for y in x])
    )
    df["antitarget_string"] = df["antitarget_messages"].apply(
        lambda x: " ".join([y["content"] for y in x])
    )

    return df


def create_conv_string(conv_array, template_name, system_prompt="", full_convo=True):
    """Create a conversation string using the specified template."""
    conv = get_conv_template(template_name)
    conv.set_system_message(system_prompt)
    if full_convo:
        for turn in conv_array:
            conv.append_message(
                conv.roles[0] if turn["role"] == "user" else conv.roles[1],
                turn["content"],
            )
    else:
        # Skip first user message
        remaining_messages = conv_array[1:]
        for turn in remaining_messages:
            conv.append_message(
                conv.roles[0] if turn["role"] == "user" else conv.roles[1],
                turn["content"],
            )
    return conv.get_prompt()


def add_conversation_strings(df, template_name="llama-3"):
    """Add formatted conversation strings using chat template."""
    print(f"Adding conversation strings (template: {template_name})...")

    system_prompt = ""

    def format_row(row):
        target_conv_str = create_conv_string(
            row["target_conv_arr"], template_name, system_prompt, full_convo=True
        )
        target_conv_str_wout_prompt = create_conv_string(
            row["target_conv_arr"], template_name, system_prompt, full_convo=False
        )
        antitarget_conv_str = create_conv_string(
            row["antitarget_conv_arr"], template_name, system_prompt, full_convo=True
        )
        antitarget_conv_str_wout_prompt = create_conv_string(
            row["antitarget_conv_arr"], template_name, system_prompt, full_convo=False
        )
        opening_prompt = f'user: {row["user_messages"][0]["content"]}'

        return (
            target_conv_str,
            target_conv_str_wout_prompt,
            antitarget_conv_str,
            antitarget_conv_str_wout_prompt,
            opening_prompt,
        )

    tqdm.pandas(desc="Formatting conversations")
    results = df.progress_apply(format_row, axis=1)

    (
        df["target_conv_str"],
        df["target_conv_str_wout_prompt"],
        df["antitarget_conv_str"],
        df["antitarget_conv_str_wout_prompt"],
        df["opening_prompt"],
    ) = zip(*results)

    # Check all message arrays are same length
    df["same_length"] = df.apply(
        lambda row: len(row["user_messages"])
        == len(row["target_messages"])
        == len(row["antitarget_messages"]),
        axis=1,
    )

    return df


def add_token_counts(df):
    """Add token counts using Llama tokenizer."""
    try:
        from huggingface_hub import login
        from transformers import AutoTokenizer
    except ImportError:
        print("Error: transformers and huggingface_hub required for tokenization")
        print("Install with: pip install transformers huggingface_hub")
        return df

    hf_token = os.environ.get("HF_TOKEN")
    if not hf_token:
        print("Error: HF_TOKEN environment variable not set")
        print("Set with: export HF_TOKEN=your-token-here")
        return df

    print("Loading Llama tokenizer...")
    login(token=hf_token)
    tokenizer = AutoTokenizer.from_pretrained("meta-llama/Llama-3.1-70B-Instruct")

    def count_tokens(text):
        return len(tokenizer(text)["input_ids"])

    print("Counting tokens...")
    tqdm.pandas(desc="Target tokens")
    df["n_target_tokens"] = df["target_string"].progress_apply(count_tokens)

    tqdm.pandas(desc="Antitarget tokens")
    df["n_antitarget_tokens"] = df["antitarget_string"].progress_apply(count_tokens)

    return df


def main():
    parser = argparse.ArgumentParser(description="Process parsed test cases")
    parser.add_argument(
        "--test",
        action="store_true",
        help="Test mode: process limited data, print results, no files written",
    )
    parser.add_argument(
        "--input-dir",
        default="data/processed_cases",
        help="Directory containing parsed case files",
    )
    parser.add_argument(
        "--output-dir",
        default="data/processed_cases",
        help="Directory to save processed cases",
    )
    parser.add_argument(
        "--tokenize",
        action="store_true",
        help="Add token counts using Llama tokenizer (requires HF_TOKEN)",
    )
    parser.add_argument(
        "--template",
        default="llama-3",
        help="Conversation template name for formatting (default: llama-3)",
    )
    args = parser.parse_args()

    setup_logging()

    # Step 1: Load and concatenate
    print("Loading parsed cases...")
    df = load_and_concatenate(args.input_dir)
    print(f"Total cases loaded: {len(df)}")

    if args.test:
        df = df.head(10)
        print(f"Test mode: limited to {len(df)} cases")

    # Step 2: Filter valid cases
    print("\nFiltering valid cases...")
    df = filter_valid_cases(df)

    # Step 3: Explode and format
    print("\nExploding and formatting...")
    df = explode_and_format(df)

    if args.test:
        df = df.head(20)
        print(f"Test mode: limited to {len(df)} prompts after explode")

    # Step 4: Construct conversation formats
    df = construct_conversation_formats(df)

    # Step 5: Add conversation template strings
    df = add_conversation_strings(df, template_name=args.template)

    # Step 6: Optionally add token counts
    if args.tokenize:
        print("\nAdding token counts...")
        df = add_token_counts(df)

    # Summary
    print("\n" + "=" * 50)
    print(f"Final dataset: {len(df)} prompts")
    print(f"Columns: {list(df.columns)}")

    if args.test:
        print("\n=== TEST MODE: Results (not saved) ===")
        print(df.head(2).to_string())
    else:
        # Save results
        os.makedirs(args.output_dir, exist_ok=True)
        output_path = os.path.join(args.output_dir, "processed_cases.jsonl")
        print(f"\nSaving to {output_path}")
        df.to_json(output_path, orient="records", lines=True)
        print("Done.")


if __name__ == "__main__":
    main()
