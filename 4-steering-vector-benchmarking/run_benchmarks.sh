#!/bin/bash

# Array of multiplier values
multipliers=(-1.5 -1.0 -0.5 0.0 0.5 1.0 1.5)

# Since you're already in the 4-steering-vector-benchmarking directory
# We need to find where your virtual environment is located

# Assuming the virtual environment is one directory up
VENV_PATH="../steering_vecs/bin/activate"

# Check if the VENV_PATH exists, if not try common locations
if [ ! -f "$VENV_PATH" ]; then
  # Try one directory up, venv or env directory names
  for venv_dir in "../venv" "../env" "../steering_vecs" "../../venv" "../../env" "../../steering_vecs" "~/venv" "~/env" "~/steering_vecs"; do
    if [ -f "$venv_dir/bin/activate" ]; then
      VENV_PATH="$venv_dir/bin/activate"
      echo "Found virtual environment at: $VENV_PATH"
      break
    fi
  done
fi

# Create a screen session for each multiplier value
for multiplier in "${multipliers[@]}"; do
  # Create a descriptive session name
  SESSION_NAME="benchmark_${multiplier//./_}"

  # Create the screen session and run the commands
  screen -dmS "$SESSION_NAME" bash -c "source $VENV_PATH && python launch_benchmarks.py --multiplier $multiplier; exec bash"

  echo "Started benchmark with multiplier $multiplier in screen session: $SESSION_NAME"
done

echo -e "\nTo see all running screen sessions, use: screen -ls"
echo "To attach to a specific session, use: screen -r SESSION_NAME"
echo "To detach from a session (and leave it running), press: Ctrl+A followed by D"

# # If want to run one
# export OPENAI_BASE_URL=<steering vector with multiplier url>
# export OPENAI_API_KEY=<api_key>
# inspect eval inspect_evals/mmlu_0_shot --model model

# # Or run all at once
# python launch_benchmarks.py --multiplier -1.5
# python launch_benchmarks.py --multiplier -1.0
# python launch_benchmarks.py --multiplier -0.5
# python launch_benchmarks.py --multiplier 0.0
# python launch_benchmarks.py --multiplier 0.5
# python launch_benchmarks.py --multiplier 1.0
# python launch_benchmarks.py --multiplier 1.5
