#!/usr/bin/env python3

import json
import os
import subprocess
import sys
import glob
from pathlib import Path

# Path to the deployments file (create from template in 3-steering-vector-hosting/)
DEPLOYMENTS_FILE = "deployments.jsonl"
# Path to the benchmarks file
BENCHMARKS_FILE = "benchmarks.jsonl"
# Base directory for logs
LOGS_DIR = "logs"


# # Set initial limit for all benchmarks
# INITIAL_LIMIT = 1000
def check_benchmark_already_run(multiplier_dir, benchmark_name, eval_name):
    """Check if a benchmark has already been run by looking for its log file"""
    # Extract the last part of the eval_name path and handle possible format differences
    eval_short_name = os.path.basename(eval_name)

    # Create alternative formats that might be in the filename (convert underscores to hyphens)
    alternative_name = eval_short_name.replace("_", "-")

    # Special handling for XSTest which needs both safe and unsafe files
    if benchmark_name == "XSTest":
        # Use both original and alternative format in patterns
        safe_patterns = [
            os.path.join(multiplier_dir, f"*{eval_short_name}*safe*.json"),
            os.path.join(multiplier_dir, f"*{alternative_name}*safe*.json"),
        ]
        unsafe_patterns = [
            os.path.join(multiplier_dir, f"*{eval_short_name}*unsafe*.json"),
            os.path.join(multiplier_dir, f"*{alternative_name}*unsafe*.json"),
        ]

        safe_files = []
        unsafe_files = []
        for pattern in safe_patterns:
            safe_files.extend(glob.glob(pattern))
        for pattern in unsafe_patterns:
            unsafe_files.extend(glob.glob(pattern))

        # Only consider XSTest complete if both safe and unsafe files exist
        if safe_files and unsafe_files:
            print(
                f"Benchmark {benchmark_name} already has both safe and unsafe log files in {multiplier_dir}:"
            )
            for file in safe_files + unsafe_files:
                print(f"  - {os.path.basename(file)}")
            return True
        elif safe_files or unsafe_files:
            print(f"Benchmark {benchmark_name} has partial results (will run again):")
            for file in safe_files + unsafe_files:
                print(f"  - {os.path.basename(file)}")
            return False
        return False
    else:
        # Standard check for other benchmarks - try both original and alternative format
        patterns = [
            os.path.join(multiplier_dir, f"*{eval_short_name}*.json"),
            os.path.join(multiplier_dir, f"*{alternative_name}*.json"),
        ]

        matching_files = []
        for pattern in patterns:
            matching_files.extend(glob.glob(pattern))

        if matching_files:
            print(
                f"Benchmark {benchmark_name} already has log files in {multiplier_dir}:"
            )
            for file in matching_files:
                print(f"  - {os.path.basename(file)}")
            return True
        return False


def run_xstest_subset(url, api_key, multiplier_dir, eval_name, subset, limit=None):
    """Run a specific subset (safe/unsafe) of XSTest"""
    print(f"Running XSTest {subset} subset...")

    # Create a new environment with the API credentials
    env = os.environ.copy()
    env["OPENAI_BASE_URL"] = url
    env["OPENAI_API_KEY"] = api_key

    # Prepare the command
    cmd = [
        "inspect",
        "eval",
        eval_name,
        "--model",
        "openai/model",
        "--temperature",
        "0.0",
        "--log-format",
        "json",
        "--log-dir",
        multiplier_dir,
        "--max-connections",
        "20",
        "--timeout",
        "100",
        "--max-tokens",
        "1000",
        "-T",
        f"subset={subset}",
        "-T",
        "scorer_model=anthropic/claude-3-7-sonnet-20250219",
    ]

    # Add limit if provided
    if limit is not None:
        cmd.extend(["--limit", str(limit)])

    # Run the command with the UI visible
    try:
        print(f"Executing: {' '.join(cmd)}")
        process = subprocess.Popen(cmd, env=env)
        return_code = process.wait()

        if return_code == 0:
            print(f"XSTest {subset} subset completed successfully")
            return True
        else:
            print(f"XSTest {subset} subset failed with exit code {return_code}")
            return False
    except Exception as e:
        print(f"Error running XSTest {subset} subset: {str(e)}")
        return False
    finally:
        print("--------------------------------------------------")


def run_benchmark(url, api_key, multiplier, benchmark):
    """Run a single benchmark for a specific deployment"""

    # Extract benchmark information
    benchmark_name = benchmark.get("name")
    eval_name = benchmark.get("eval_name")
    category = benchmark.get("category", "Uncategorized")
    no_max_tokens = benchmark.get("no_max_tokens", False)

    print(
        f"Running {category} benchmark: {benchmark_name} with multiplier {multiplier}..."
    )

    # Create directory structure: logs/category/multiplier_X/
    category_dir = os.path.join(LOGS_DIR, category)
    multiplier_dir = os.path.join(category_dir, f"multiplier_{multiplier}")
    os.makedirs(multiplier_dir, exist_ok=True)

    # Check if this benchmark has already been run
    if check_benchmark_already_run(multiplier_dir, benchmark_name, eval_name):
        print(
            f"Skipping benchmark {benchmark_name} for multiplier {multiplier} as it has already been run"
        )
        return True

    # Create a new environment with the API credentials
    env = os.environ.copy()
    env["OPENAI_BASE_URL"] = url
    env["OPENAI_API_KEY"] = api_key

    if benchmark_name == "XSTest":
        # XSTest is run in two parts: safe and unsafe subsets
        success_safe = run_xstest_subset(
            url, api_key, multiplier_dir, eval_name, "safe", None
        )
        success_unsafe = run_xstest_subset(
            url, api_key, multiplier_dir, eval_name, "unsafe", None
        )
        return success_safe and success_unsafe
    else:
        cmd = [
            "inspect",
            "eval",
            eval_name,
            "--model",
            "openai/model",
            "--temperature",
            "0.0",
            "--log-format",
            "json",
            "--max-connections",
            "20",
            "--timeout",
            "100",
            "--max-tokens",
            "1000",
            "--log-dir",
            multiplier_dir,
        ]

        # Run the command with the UI visible
        try:
            print(f"Executing: {' '.join(cmd)}")
            process = subprocess.Popen(cmd, env=env)
            return_code = process.wait()

            if return_code == 0:
                print(
                    f"Benchmark {benchmark_name} completed successfully for multiplier {multiplier}"
                )
                return True
            else:
                print(
                    f"Benchmark {benchmark_name} failed for multiplier {multiplier} with exit code {return_code}"
                )
                return False
        except Exception as e:
            print(f"Error running benchmark {benchmark_name}: {str(e)}")
            return False
        finally:
            print("--------------------------------------------------")


def main():
    # Parse command line arguments
    import argparse

    parser = argparse.ArgumentParser(
        description="Run benchmarks for a specific multiplier"
    )
    parser.add_argument(
        "--multiplier", type=float, help="Specific multiplier to run (e.g., 1.0)"
    )
    parser.add_argument(
        "--deployments",
        type=str,
        default=DEPLOYMENTS_FILE,
        help="Path to deployments.jsonl file (default: deployments.jsonl)",
    )
    args = parser.parse_args()

    # Use deployments file from args
    deployments_file = args.deployments

    # Check if the deployments file exists
    if not Path(deployments_file).is_file():
        print(f"Error: Deployments file not found at {deployments_file}")
        print(
            "Create one from the template in 3-steering-vector-hosting/deployments_template.jsonl"
        )
        sys.exit(1)

    # Check if the benchmarks file exists
    if not Path(BENCHMARKS_FILE).is_file():
        print(f"Error: Benchmarks file not found at {BENCHMARKS_FILE}")
        sys.exit(1)

    target_multiplier = args.multiplier
    if target_multiplier is not None:
        print(f"Running benchmarks only for multiplier: {target_multiplier}")
    else:
        print("No specific multiplier provided, will run for all multipliers")

    # Load benchmarks
    benchmarks = []
    with open(BENCHMARKS_FILE, "r") as f:
        for line in f:
            line = line.strip()
            if not line:  # Skip empty lines
                continue

            try:
                benchmark = json.loads(line)
                benchmarks.append(benchmark)
            except json.JSONDecodeError:
                print(f"Error: Invalid JSON in benchmarks line: {line}")

    # Create logs directory if it doesn't exist
    os.makedirs(LOGS_DIR, exist_ok=True)

    # Keep track of successful and failed benchmarks
    results = {}

    # Process each line in the deployments file
    with open(deployments_file, "r") as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:  # Skip empty lines
                continue

            try:
                # Parse the JSON line
                deployment = json.loads(line)

                # Extract the required values
                url = deployment.get("url")
                api_key = deployment.get("key")
                multiplier = deployment.get("multiplier")

                # Skip if not matching the target multiplier (if specified)
                if (
                    target_multiplier is not None
                    and float(multiplier) != target_multiplier
                ):
                    continue

                if not url or not api_key:
                    print(
                        f"Error: Missing URL or API key in deployment line {line_num}"
                    )
                    continue

                # Initialize results for this multiplier
                multiplier_key = f"multiplier_{multiplier}"
                if multiplier_key not in results:
                    results[multiplier_key] = {
                        "successful": [],
                        "failed": [],
                        "skipped": [],
                    }

                # Run each benchmark for this deployment
                for benchmark in benchmarks:
                    # Create the paths to check if benchmark was already run
                    category = benchmark.get("category", "Uncategorized")
                    category_dir = os.path.join(LOGS_DIR, category)
                    multiplier_dir = os.path.join(
                        category_dir, f"multiplier_{multiplier}"
                    )

                    # Only run if benchmark hasn't been run already
                    if check_benchmark_already_run(
                        multiplier_dir, benchmark["name"], benchmark["eval_name"]
                    ):
                        results[multiplier_key]["skipped"].append(benchmark["name"])
                        continue

                    success = run_benchmark(url, api_key, multiplier, benchmark)
                    if success:
                        results[multiplier_key]["successful"].append(benchmark["name"])
                    else:
                        results[multiplier_key]["failed"].append(benchmark["name"])

            except json.JSONDecodeError:
                print(f"Error: Invalid JSON in deployment line {line_num}")
            except Exception as e:
                print(f"Error processing deployment line {line_num}")
                print(f"Exception: {str(e)}")

    # Print summary
    print("\n===== Benchmark Execution Summary =====")
    for multiplier, outcome in results.items():
        print(f"\n{multiplier}:")
        print(f"  Successful benchmarks ({len(outcome['successful'])}):")
        for benchmark in outcome["successful"]:
            print(f"    - {benchmark}")

        if outcome["failed"]:
            print(f"  Failed benchmarks ({len(outcome['failed'])}):")
            for benchmark in outcome["failed"]:
                print(f"    - {benchmark}")

        if outcome.get("skipped"):
            print(f"  Skipped benchmarks (already exist) ({len(outcome['skipped'])}):")
            for benchmark in outcome["skipped"]:
                print(f"    - {benchmark}")

    print("\nAll benchmarks completed.")


if __name__ == "__main__":
    main()
