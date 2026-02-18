#!/usr/bin/env python
"""Generate steered responses using trained vectors across multiple steering multipliers."""

import argparse
import json
import os
from datetime import datetime

import numpy as np
import pandas as pd
import torch
from accelerate import Accelerator
from tqdm import tqdm

# Import model wrapper from parent directory
import sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from BiPO_distributed.model_wrapper import ModelWrapper


def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)


def calculate_sequence_logprob(model, sequence, device):
    """Calculate mean log probability for a sequence."""
    tokens = model.tokenizer(sequence["content"], return_tensors="pt").input_ids.to(
        device
    )
    with torch.no_grad():
        logits = model.get_logits(tokens)
        log_probs = torch.nn.functional.log_softmax(logits, dim=-1)
        token_log_probs = (
            log_probs[:, :-1].gather(-1, tokens[:, 1:].unsqueeze(-1)).squeeze(-1)
        )
        return token_log_probs.mean().item()


def main():
    # Initialize accelerator
    accelerator = Accelerator()
    DEVICE = accelerator.device
    rank = accelerator.process_index

    print(f"Accelerator initialized. Device: {accelerator.device}, Rank: {accelerator.process_index}")
    print(f"CUDA available: {torch.cuda.is_available()}")
    if torch.cuda.is_available():
        print(f"CUDA device count: {torch.cuda.device_count()}")
        print(f"Current CUDA device: {torch.cuda.current_device()}")

    # Parse CLI arguments
    parser = argparse.ArgumentParser(description="Multi-GPU Generation Evaluation")
    parser.add_argument(
        "--model_name", type=str, default="meta-llama/Llama-3.1-8B-Instruct"
    )
    parser.add_argument("--layer", type=int, default=15)
    parser.add_argument("--epoch", type=int, default=20)
    parser.add_argument(
        "--gpu_id", type=int, default=3, help="Should match the saved vector's rank"
    )
    parser.add_argument("--behavior", type=str, default="relationship-seeking")
    args = parser.parse_args()

    # Load test data from shared data folder (paths relative to repo root)
    data_dir = f"data/{args.behavior}"
    array_df = pd.read_json(os.path.join(data_dir, "test.jsonl"), lines=True)
    array_df = array_df[
        ["prompt_id", "prompt_arr", "chosen_arr", "rejected_arr", "assistant_turn_idx"]
    ]

    print(f"Number of test rows: {len(array_df)}")
    print(
        f"Distribution of turns : {array_df['assistant_turn_idx'].value_counts().sort_index()}"
    )

    TEST_PROMPTS = array_df["prompt_arr"].to_list()
    TEST_PROMPT_IDS = array_df["prompt_id"].to_list()
    CHOSEN_RESPONSES = array_df["chosen_arr"].to_list()
    REJECTED_RESPONSES = array_df["rejected_arr"].to_list()

    # Print test prompt
    print(f"Test prompt: {TEST_PROMPTS[0]}")
    print(f"Chosen response: {CHOSEN_RESPONSES[0]}")
    print(f"Rejected response: {REJECTED_RESPONSES[0]}")

    # Multiplier range
    MULTIPLIERS = [-20, -10, -5, -4, -3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 10, 20]

    # Setup output directory (relative to repo root)
    short_model_name = args.model_name.split("/")[-1]
    candidate_dir = os.path.join("vector_evals", short_model_name, f"layer{args.layer}")
    ensure_dir(candidate_dir)

    # Setup output file
    if accelerator.num_processes > 1:
        OUTPUT_FILE = os.path.join(
            candidate_dir, f"generations_ep{args.epoch}_rank{rank}.jsonl"
        )
    else:
        OUTPUT_FILE = os.path.join(candidate_dir, f"generations_ep{args.epoch}.jsonl")

    # Load completed records
    completed_records = set()
    completed_ids = set()
    if os.path.exists(OUTPUT_FILE):
        with open(OUTPUT_FILE, "r") as f_existing:
            for line in f_existing:
                try:
                    record = json.loads(line)
                    key = (record["test_prompt_id"], record["multiplier"])
                    completed_records.add(key)
                    completed_ids.add(record["generation_id"])
                except Exception as e:
                    print(f"[Warning] Failed to parse a line in {OUTPUT_FILE}: {e}")
    if len(completed_records) > 0:
        print(f"Loaded {len(completed_records)} completed records")
        print(f"Last completed generation_id: {max(completed_ids)}")
        gen_idx = max(completed_ids) + 1
    else:
        print("No completed records found, starting from scratch")
        gen_idx = 0

    # Initialize model
    SYSTEM_PROMPT = "You are a conversational assistant."
    model = ModelWrapper(
        token=os.environ.get("HF_TOKEN"),
        system_prompt=SYSTEM_PROMPT,
        model_name_or_path=args.model_name,
    )
    model.model = accelerator.prepare(model.model)

    # Load steering vector (relative to repo root)
    vector_path = f"vector/{args.behavior}_{short_model_name}/layer{args.layer}/vec_ep{args.epoch}_layer{args.layer}_gpu{args.gpu_id}.pt"
    vector = torch.load(vector_path, map_location=DEVICE).to(dtype=model.model.dtype)
    vector = vector.view(1, -1)

    # Save start time
    start_time = datetime.now().isoformat()

    with open(OUTPUT_FILE, "a") as f:
        # Create iterator for all data
        data_iterator = zip(
            TEST_PROMPTS, TEST_PROMPT_IDS, CHOSEN_RESPONSES, REJECTED_RESPONSES
        )

        # Only show progress bar on main process
        if rank == 0:
            pbar = tqdm(
                data_iterator, total=len(TEST_PROMPTS), desc="Processing test cases"
            )
            data_iterator = pbar
        try:
            for test_input, prompt_id, chosen, rejected in data_iterator:
                for multiplier in MULTIPLIERS:
                    if (prompt_id, multiplier) in completed_records:
                        print(f"Skipping prompt {prompt_id} with multiplier {multiplier}")
                        continue

                    model.reset_all()
                    model.set_add_activations(args.layer, multiplier * vector)

                    # 1. Generation
                    with torch.no_grad():
                        tokens_out = model.generate_text_with_conversation_history(
                            history=test_input, max_new_tokens=100, decode=False
                        )

                        string_out = model.tokenizer.decode(
                            tokens_out, skip_special_tokens=True
                        )

                        # 2. Perplexity calculation
                        tokens_out = tokens_out.to(DEVICE)
                        tokens_batch = (
                            tokens_out.unsqueeze(0) if tokens_out.dim() == 1 else tokens_out
                        )
                        logits = model.get_logits(tokens_batch)
                        log_probs = torch.nn.functional.log_softmax(logits, dim=-1)
                        token_log_probs = (
                            log_probs[:, :-1]
                            .gather(-1, tokens_batch[:, 1:].unsqueeze(-1))
                            .squeeze(-1)
                        )
                        avg_log_prob = token_log_probs.mean()
                        perplexity = torch.exp(-avg_log_prob).item()

                    # 3. Calculate log probs for chosen and rejected
                    chosen_logprob = calculate_sequence_logprob(model, chosen[0], DEVICE)
                    rejected_logprob = calculate_sequence_logprob(model, rejected[0], DEVICE)
                    propensity = chosen_logprob - rejected_logprob

                    # Clean up
                    del (
                        logits,
                        log_probs,
                        token_log_probs,
                    )
                    torch.cuda.empty_cache()

                    # Save results
                    result = {
                        "test_prompt_id": prompt_id,
                        "timestamp": datetime.now().isoformat(),
                        "model_name": short_model_name,
                        "epoch": args.epoch,
                        "layer": args.layer,
                        "multiplier": multiplier,
                        "generation_id": gen_idx,
                        "response_str": string_out,
                        "response_tokens": tokens_out.cpu().numpy().tolist(),
                        "avg_log_prob": avg_log_prob.item(),
                        "perplexity": perplexity,
                        "chosen_logprob": chosen_logprob,
                        "rejected_logprob": rejected_logprob,
                        "propensity": propensity,
                    }
                    f.write(json.dumps(result) + "\n")
                    f.flush()

                    completed_records.add((prompt_id, multiplier))
                    gen_idx += 1
        finally:
            if rank == 0:
                pbar.close()

    # Log experiment completion
    print("Experiment completed, adding to log")
    if accelerator.is_main_process:
        log_entry = {
            "short_model": short_model_name,
            "layer": args.layer,
            "n_test_prompts": len(TEST_PROMPTS),
            "n_multipliers": len(MULTIPLIERS),
            "n_generations": gen_idx,
            "start_time": start_time,
            "end_time": datetime.now().isoformat(),
            "duration (min)": (
                (datetime.now() - datetime.fromisoformat(start_time)).seconds
            )
            / 60,
        }

        with open("./vector_evals/experiment_log.jsonl", "a") as f:
            f.write(json.dumps(log_entry) + "\n")

    # Synchronize processes
    accelerator.wait_for_everyone()


if __name__ == "__main__":
    main()
