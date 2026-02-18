#!/usr/bin/env python3
import argparse
import json
import os
import random
import sys
from pathlib import Path

from tqdm import tqdm
from openai import OpenAI

# Add parent directories to path for imports
script_dir = Path(__file__).parent
sys.path.insert(0, str(script_dir.parent.parent))  # 2-steering-vector-training/
sys.path.insert(0, str(script_dir.parent.parent.parent))  # relationship-seeking-ai/

from setup.api_utils import get_api_key
from scripts.eval_rating import ScoringSystem, simple_conv_history_processer
from scripts.utils.exp_cost_tracking import calculate_cost
from stability_scenarios import THEMES

def main():
    parser = argparse.ArgumentParser(description="Score persona stability experiment results")
    parser.add_argument('--results-dir', default='persona_stability_experiments')
    parser.add_argument('--output-dir', default='persona_stability_experiments')
    parser.add_argument('--sample-ratio', type=float, default=1.0)
    parser.add_argument('--model', choices=['gpt', 'claude', 'llama_70B-layer31-ep10'], required=True)
    parser.add_argument('--themes', nargs='+', choices=THEMES, default=THEMES,
                        help="Which themes to score; defaults to all")
    parser.add_argument('--task', default='relationship',
                        choices=['coherence', 'relationship'],
                        help="Scoring task/rubric to use (default: relationship)")
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)
    client = OpenAI(api_key=get_api_key('OPENAI_API_KEY'))
    scorer = ScoringSystem()
    total_cost = 0.0

    for theme in args.themes:
        input_path = Path(args.results_dir) / f"persona_stability_{args.model}_{theme}.jsonl"
        if not input_path.exists():
            print(f"Skipping missing file: {input_path}")
            continue
        output_file = Path(args.output_dir) / f"persona_stability_scores_{args.model}_{theme}.jsonl"

        existing_ids = set()
        if output_file.exists():
            with open(output_file) as f:
                for line in f:
                    try:
                        existing_ids.add(json.loads(line).get('experiment_id'))
                    except:
                        pass

        experiments = [json.loads(l) for l in open(input_path)]
        if args.sample_ratio < 1.0:
            experiments = random.sample(experiments, max(1, int(len(experiments) * args.sample_ratio)))

        with open(output_file, 'a') as out_f:
            for exp_idx, exp in enumerate(tqdm(experiments, desc=f"Scoring {args.model}/{theme}")):
                exp_id = f"{args.model}_{exp['scenario_name']}_{exp['multiplier']}_{exp_idx}"
                if exp_id in existing_ids:
                    continue

                conv = exp['conversation_history']
                attack_type = exp['scenario_name'].split('_')[0]
                multiplier = exp.get('multiplier')

                assistant_turn_idx = 0
                for i, turn in enumerate(conv):
                    if turn['role'] == 'system' or turn['role'] != 'assistant':
                        continue

                    history = simple_conv_history_processer(conv[:i])
                    response = turn['content']
                    prev_prompt = conv[i-1]['content'] if i > 0 and conv[i-1]['role'] == 'user' else ''

                    try:
                        score, usage = scorer.score_response(
                            client,
                            history_str=history,
                            response_str=response,
                            task=args.task,
                            single_example=False,
                            example_idx=0,
                            RUN_INDEX=22
                        )
                    except Exception as e:
                        print(f"Error scoring {exp_id} turn {i}: {e}")
                        score, usage = None, None

                    cost = usage.get('cost', {}).get('total_cost', 0.0) if usage else 0.0
                    total_cost += cost

                    record = {
                        'experiment_id': exp_id,
                        'model': args.model,
                        'theme': theme,
                        'multiplier': multiplier,
                        'attack_type': attack_type,
                        'turn_index': assistant_turn_idx,
                        'score': score,
                        'response_text': response,
                        'prompt_text': prev_prompt
                    }
                    out_f.write(json.dumps(record) + '\n')
                    out_f.flush()
                    assistant_turn_idx += 1

    print(f"Done. Total scoring cost: ${total_cost:.4f}")

if __name__ == '__main__':
    main()
