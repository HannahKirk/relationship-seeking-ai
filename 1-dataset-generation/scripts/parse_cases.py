#!/usr/bin/env python3
"""
Parse raw LLM-generated test cases and fix broken JSON.

This script:
1. Loads raw cases from all models
2. Attempts programmatic JSON parsing/repair
3. Uses Claude API to fix remaining broken cases
4. Saves parsed cases to data/processed_cases/
"""

import argparse
import json
import logging
import os
import re
import sys

import jsonlines
import pandas as pd
from tqdm import tqdm

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from anthropic import Anthropic
from setup.api_utils import get_api_key
from utils.generation_utils import setup_logging


def extract_json_from_raw_output(raw_output):
    """
    Extract valid JSON from raw output text that may contain markdown code blocks,
    extra text, or other formatting.
    """
    if not isinstance(raw_output, str):
        return None

    # 1. Try direct parsing
    try:
        return json.loads(raw_output)
    except (json.JSONDecodeError, TypeError):
        pass

    # 2. Try to extract JSON from triple backticks (markdown code blocks)
    backtick_pattern = r"```(?:json)?\s*([\s\S]*?)```"
    matches = re.findall(backtick_pattern, raw_output)
    for match in matches:
        try:
            return json.loads(match.strip())
        except json.JSONDecodeError:
            continue

    # 3. Try to find JSON starting with '{' and ending with '}'
    brace_pattern = r"(\{[\s\S]*\})"
    matches = re.findall(brace_pattern, raw_output)
    for match in matches:
        try:
            return json.loads(match.strip())
        except json.JSONDecodeError:
            continue

    # 4. Try to find JSON starting with '[' and ending with ']'
    array_pattern = r"(\[[\s\S]*\])"
    matches = re.findall(array_pattern, raw_output)
    for match in matches:
        try:
            return json.loads(match.strip())
        except json.JSONDecodeError:
            continue

    return None


def recursive_json_decode(obj):
    """
    Recursively decode JSON strings within objects.
    Handles cases where JSON is nested inside other JSON strings.
    """
    if isinstance(obj, dict):
        for key in list(obj.keys()):
            val = obj[key]
            if isinstance(val, str) and (
                (val.startswith("{") and val.endswith("}"))
                or (val.startswith("[") and val.endswith("]"))
            ):
                try:
                    decoded = json.loads(val)
                    obj[key] = recursive_json_decode(decoded)
                except json.JSONDecodeError:
                    pass
            elif isinstance(val, (dict, list)):
                obj[key] = recursive_json_decode(val)

    elif isinstance(obj, list):
        for i in range(len(obj)):
            val = obj[i]
            if isinstance(val, str) and (
                (val.startswith("{") and val.endswith("}"))
                or (val.startswith("[") and val.endswith("]"))
            ):
                try:
                    decoded = json.loads(val)
                    obj[i] = recursive_json_decode(decoded)
                except json.JSONDecodeError:
                    pass
            elif isinstance(val, (dict, list)):
                obj[i] = recursive_json_decode(val)

    return obj


def process_dataframe_conversations(df):
    """
    Process a DataFrame to fix broken conversations columns from raw_output.
    Fills 'conversations' with parsed data, sets 'parsing_success' = True/False.
    """
    result_df = df.copy()
    if "parsing_success" not in result_df.columns:
        result_df["parsing_success"] = False

    total_records = len(result_df)
    already_parsed = 0
    successfully_fixed = 0
    failed_to_parse = 0

    for i in tqdm(range(total_records), desc="Processing rows"):
        row = result_df.iloc[i]
        idx = result_df.index[i]

        conv_data = row.get("conversations", None)
        raw_output = row.get("raw_output", None)

        # If conversations is already a list or dict, check for nested JSON
        if isinstance(conv_data, (list, dict)):
            try:
                result_df.at[idx, "conversations"] = recursive_json_decode(conv_data)
                result_df.at[idx, "parsing_success"] = True
                already_parsed += 1
            except Exception as e:
                logging.error(
                    f"Row {i} (index {idx}): Error decoding nested JSON: {str(e)}"
                )
                result_df.at[idx, "parsing_success"] = False
                failed_to_parse += 1
            continue

        # If conv_data is a string that might be JSON
        if isinstance(conv_data, str) and (
            conv_data.startswith("[") or conv_data.startswith("{")
        ):
            try:
                json_obj = json.loads(conv_data)
                json_obj = recursive_json_decode(json_obj)
                result_df.at[idx, "conversations"] = json_obj
                result_df.at[idx, "parsing_success"] = True
                already_parsed += 1
                continue
            except json.JSONDecodeError:
                pass

        # If we have raw_output, try to parse that
        if raw_output:
            try:
                parsed_json = extract_json_from_raw_output(raw_output)
                if parsed_json and "conversations" in parsed_json:
                    if isinstance(parsed_json["conversations"], str):
                        try:
                            conv_obj = json.loads(parsed_json["conversations"])
                            conv_obj = recursive_json_decode(conv_obj)
                            parsed_json["conversations"] = conv_obj
                        except json.JSONDecodeError:
                            result_df.at[idx, "parsing_success"] = False
                            failed_to_parse += 1
                            continue

                    parsed_json = recursive_json_decode(parsed_json)
                    result_df.at[idx, "conversations"] = parsed_json["conversations"]
                    result_df.at[idx, "parsing_success"] = True
                    successfully_fixed += 1
                else:
                    result_df.at[idx, "parsing_success"] = False
                    failed_to_parse += 1
            except Exception as e:
                logging.error(f"Row {i} (index {idx}): Error - {str(e)}")
                result_df.at[idx, "parsing_success"] = False
                failed_to_parse += 1
        else:
            result_df.at[idx, "parsing_success"] = False
            failed_to_parse += 1

    print("Processing complete")
    print(f"Total records: {total_records}")
    print(f"Already parsed: {already_parsed}")
    print(f"Successfully fixed: {successfully_fixed}")
    print(f"Failed to parse: {failed_to_parse}")

    return result_df


def aggressive_json_extraction(raw_output):
    """
    Tries to salvage even badly corrupted JSON.
    """
    if not isinstance(raw_output, str):
        return None

    standard_result = extract_json_from_raw_output(raw_output)
    if standard_result:
        return standard_result

    # Look for conversation blocks
    conversation_objects = []
    conv_pattern = r'(\{\s*"reasoning"[\s\S]*?"conversation"[\s\S]*?\}\s*\])'
    matches = re.findall(conv_pattern, raw_output)
    if matches:
        for match in matches:
            try:
                cleaned = re.sub(r",\s*\]", "]", match.strip())
                cleaned = re.sub(r",\s*\}", "}", cleaned)
                if not cleaned.endswith("}"):
                    cleaned = cleaned.rstrip("]")
                    if not cleaned.endswith("}"):
                        cleaned += "}"
                conv_obj = json.loads(cleaned)
                conversation_objects.append(conv_obj)
            except json.JSONDecodeError:
                pass
        if conversation_objects:
            return {"conversations": conversation_objects}

    # Look for "conversations" array
    conv_array_pattern = r'"conversations"\s*:\s*(\[[\s\S]*?\])'
    matches = re.findall(conv_array_pattern, raw_output)
    for match in matches:
        try:
            cleaned = re.sub(r",\s*\]", "]", match.strip())
            conv_arr = json.loads(cleaned)
            if isinstance(conv_arr, list):
                return {"conversations": conv_arr}
        except json.JSONDecodeError:
            pass

    # Minimal salvage from key patterns
    reasoning_matches = re.findall(r'"reasoning"\s*:\s*"([^"]*)"', raw_output)
    user_message_matches = re.findall(r'"user_message"\s*:\s*"([^"]*)"', raw_output)
    target_matches = re.findall(r'"target_response"\s*:\s*"([^"]*)"', raw_output)
    antitarget_matches = re.findall(
        r'"antitarget_response"\s*:\s*"([^"]*)"', raw_output
    )

    if reasoning_matches and user_message_matches:
        reconstructed = {"conversations": []}
        reasoning = reasoning_matches[0]
        count_turns = min(
            len(user_message_matches), len(target_matches), len(antitarget_matches)
        )
        for i in range(count_turns):
            conversation_obj = {
                "reasoning": reasoning,
                "conversation": [
                    {
                        "user_message": user_message_matches[i],
                        "target_response": (
                            target_matches[i]
                            if i < len(target_matches)
                            else "[Missing]"
                        ),
                        "antitarget_response": (
                            antitarget_matches[i]
                            if i < len(antitarget_matches)
                            else "[Missing]"
                        ),
                    }
                ],
            }
            reconstructed["conversations"].append(conversation_obj)
        if reconstructed["conversations"]:
            return reconstructed

    return None


def fix_problematic_records(df):
    """
    Apply aggressive JSON extraction to fix previously failing records.
    """
    result_df = df.copy()
    failed_records = result_df[result_df["parsing_success"] == False]
    fixed_count = 0

    for idx, row in tqdm(
        failed_records.iterrows(),
        total=len(failed_records),
        desc="Fixing problematic records",
    ):
        raw_output = row.get("raw_output", None)
        if not raw_output:
            continue

        try:
            parsed_json = aggressive_json_extraction(raw_output)
            if parsed_json and "conversations" in parsed_json:
                if isinstance(parsed_json["conversations"], str):
                    try:
                        parsed_obj = json.loads(parsed_json["conversations"])
                        result_df.at[idx, "conversations"] = recursive_json_decode(
                            parsed_obj
                        )
                    except json.JSONDecodeError:
                        result_df.at[idx, "parsing_success"] = False
                        continue
                else:
                    result_df.at[idx, "conversations"] = recursive_json_decode(
                        parsed_json["conversations"]
                    )
                result_df.at[idx, "parsing_success"] = True
                fixed_count += 1
        except Exception as e:
            print(f"Error fixing record {idx}: {str(e)}")

    print(f"Fixed {fixed_count} additional problematic records")
    return result_df


def fix_with_claude(df, test_mode=False):
    """
    Use Claude API to fix remaining broken JSON records.

    Returns:
        tuple: (updated_df, llm_fixed_indices) - dataframe and list of indices fixed by LLM
    """
    still_broken_df = df[df["parsing_success"] == False].copy()
    print(f"{len(still_broken_df)} records still broken after programmatic repairs.")

    if len(still_broken_df) == 0:
        return df, []

    try:
        client = Anthropic(api_key=get_api_key("ANTHROPIC_API_KEY"))
    except ValueError as e:
        print(f"Skipping Claude-based fix: {e}")
        return df, []

    system_prompt = (
        "TASK INSTRUCTIONS: I will give you a malformed JSON string. You MUST return a valid JSON with the exact same content.\n"
        "The valid JSON must have the following structure:\n\n"
        "{\n"
        '  "conversations": [\n'
        "    {\n"
        '      "reasoning": "string that explains the rationale for this test case",\n'
        '      "conversation": [\n'
        "        {\n"
        '          "user_message": "message from the user",\n'
        '          "target_response": "response demonstrating relationship-seeking behavior",\n'
        '          "antitarget_response": "response demonstrating professional boundaries"\n'
        "        },\n"
        "        ... more conversation turns ...\n"
        "      ]\n"
        "    },\n"
        "    ... more test cases ...\n"
        "  ]\n"
        "}\n\n"
        "Be especially careful when there are strings within the user_message, target_response, or antitarget_response fields. "
        "These need to be properly escaped or replaced with characters that won't break the JSON.\n"
        "Provide ONLY the JSON output. Do not include any explanations, preambles, or text before or after the JSON.\n"
        "If the JSON is broken because it is truncated or unfinished, complete it as best you can.\n"
    )

    result_df = df.copy()
    fixed_count = 0
    llm_fixed_indices = []

    if test_mode:
        # Only process first broken record in test mode
        still_broken_df = still_broken_df.head(1)
        print("Test mode: processing only first broken record")

    for idx, row in tqdm(
        still_broken_df.iterrows(),
        total=len(still_broken_df),
        desc="Fixing with Claude",
    ):
        raw_output = row.get("raw_output", None)
        if not raw_output:
            continue

        try:
            response = client.messages.create(
                model="claude-3-7-sonnet-20250219",
                max_tokens=16384,
                messages=[{"role": "user", "content": raw_output}],
                system=system_prompt,
                temperature=1.0,
            )

            claude_fixed_json_str = response.content[0].text

            try:
                claude_parsed = json.loads(claude_fixed_json_str)
                result_df.at[idx, "conversations"] = claude_parsed.get(
                    "conversations", []
                )
                result_df.at[idx, "parsing_success"] = True
                llm_fixed_indices.append(idx)
                fixed_count += 1
            except json.JSONDecodeError:
                result_df.at[idx, "parsing_success"] = False

        except Exception as e:
            logging.error(f"Anthropic error fixing row {idx}: {str(e)}")
            result_df.at[idx, "parsing_success"] = False

    print(f"Claude fixed {fixed_count} additional records")
    return result_df, llm_fixed_indices


def load_raw_cases(input_dir):
    """Load and combine raw cases from all models."""
    models = ["gpt", "claude", "llama_3.1_70b"]
    frames = []

    for model in models:
        filepath = os.path.join(input_dir, f"raw_cases_{model}.jsonl")
        if os.path.exists(filepath):
            df_temp = pd.read_json(filepath, lines=True)
            frames.append(df_temp)
            print(f"Loaded {len(df_temp)} records from {model}")
        else:
            print(f"Warning: {filepath} not found")

    if not frames:
        raise FileNotFoundError(f"No raw case files found in {input_dir}")

    df = pd.concat(frames, ignore_index=True)

    # Expand token_usage and cost columns
    if "token_usage" in df.columns:
        df = pd.concat(
            [df.drop(["token_usage"], axis=1), df["token_usage"].apply(pd.Series)],
            axis=1,
        )
    if "cost" in df.columns:
        df = pd.concat(
            [df.drop(["cost"], axis=1), df["cost"].apply(pd.Series)],
            axis=1,
        )

    return df


def main():
    parser = argparse.ArgumentParser(description="Parse raw LLM-generated test cases")
    parser.add_argument(
        "--test",
        action="store_true",
        help="Test mode: process limited data, print results, no files written",
    )
    parser.add_argument(
        "--input-dir",
        default="data/raw_cases",
        help="Directory containing raw case files",
    )
    parser.add_argument(
        "--output-dir",
        default="data/processed_cases",
        help="Directory to save parsed cases",
    )
    parser.add_argument(
        "--skip-claude",
        action="store_true",
        help="Skip Claude-based fixing of broken records",
    )
    args = parser.parse_args()

    setup_logging()

    # Load raw cases
    print("Loading raw cases...")
    df = load_raw_cases(args.input_dir)
    print(f"Total records loaded: {len(df)}")

    if args.test:
        # Limit to first 10 records in test mode
        df = df.head(10)
        print(f"Test mode: limited to {len(df)} records")

    # Step 1: Programmatic parsing
    print("\nStep 1: Programmatic JSON parsing...")
    df = process_dataframe_conversations(df)

    # Step 2: Aggressive repair
    print("\nStep 2: Aggressive JSON repair...")
    df = fix_problematic_records(df)

    # Step 3: Claude-based fixing (optional)
    llm_fixed_indices = []
    if not args.skip_claude:
        print("\nStep 3: Claude-based JSON fixing...")
        df, llm_fixed_indices = fix_with_claude(df, test_mode=args.test)
    else:
        print("\nStep 3: Skipped Claude-based fixing")

    # Summary
    print("\n" + "=" * 50)
    print("Final parsing results:")
    print(df[["model", "parsing_success"]].value_counts())

    if args.test:
        print("\n=== TEST MODE: Results (not saved) ===")
        print(f"Successfully parsed: {df['parsing_success'].sum()}/{len(df)}")
        sample = df[df["parsing_success"] == True].head(1)
        if len(sample) > 0:
            print("\nSample parsed record:")
            print(json.dumps(sample.iloc[0].to_dict(), indent=2, default=str)[:1000])
    else:
        # Save results
        os.makedirs(args.output_dir, exist_ok=True)

        # Save successfully parsed cases (excluding LLM-fixed, which are saved separately)
        parsed_success_df = df[df["parsing_success"] == True]
        if llm_fixed_indices:
            parsed_success_df = parsed_success_df.drop(index=llm_fixed_indices, errors="ignore")
        output_path = os.path.join(args.output_dir, "parsed_cases.jsonl")
        print(
            f"\nSaving {len(parsed_success_df)} programmatically parsed records to {output_path}"
        )

        with jsonlines.open(output_path, "w") as writer:
            for _, row in parsed_success_df.iterrows():
                writer.write(row.to_dict())

        # Save LLM-fixed cases separately
        if llm_fixed_indices:
            llm_fixed_df = df.loc[llm_fixed_indices]
            llm_fixed_path = os.path.join(args.output_dir, "parsed_cases_llm_fixed.jsonl")
            print(f"Saving {len(llm_fixed_df)} LLM-fixed records to {llm_fixed_path}")

            with jsonlines.open(llm_fixed_path, "w") as writer:
                for _, row in llm_fixed_df.iterrows():
                    writer.write(row.to_dict())

        print("Done.")


if __name__ == "__main__":
    main()
