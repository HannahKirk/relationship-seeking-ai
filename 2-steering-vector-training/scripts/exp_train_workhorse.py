import subprocess
import time
import glob
from pathlib import Path

from exp_config import ModelConfig


def check_training_done(short_model: str, layer: int, epochs: int) -> bool:
    """Check if training is complete by looking for any vector files for this model/layer"""
    vector_pattern = f"vector/relationship-seeking_{short_model}/layer{layer}/vec_*_layer{layer}_*.pt"
    vector_files = glob.glob(vector_pattern)
    return len(vector_files) > 0


def launch_training(model_name: str, layer: int, epochs: int) -> None:
    """Launch training for a model/layer combo"""
    cmd = f"""CUDA_VISIBLE_DEVICES=0,1,2,3,4,5,6,7 accelerate launch \
        --config_file BiPO_distributed/accelerate_config.yaml \
        BiPO_distributed/train.py \
        --layer {layer} \
        --num_train_epochs {epochs} \
        --behavior relationship-seeking \
        --model_name_or_path {model_name} \
        --per_device_train_batch_size=2 \
        --gradient_accumulation_steps=2 \
        --gradient_checkpointing=True"""

    log_file = f"logs/log_train_{model_name.split('/')[-1]}_layer{layer}.log"

    with open(log_file, "w") as f:
        subprocess.Popen(
            cmd, shell=True, stdout=f, stderr=subprocess.STDOUT, executable="/bin/bash"
        )


def check_gpu_available() -> bool:
    """Check if GPUs 1-7 are available for training"""
    try:
        result = subprocess.run(
            ["nvidia-smi", "--query-gpu=memory.used", "--format=csv,noheader,nounits"],
            capture_output=True,
            text=True,
        )
        used_memory = [int(x) for x in result.stdout.strip().split("\n")]
        # Check GPUs 1-7 (index 1-7)
        return all(used_memory[i] < 1000 for i in range(1, 8))  # Less than 1GB used
    except:
        return False


def main():
    print("Starting Training Workhorse...")

    while True:
        print("\nChecking experiment status...")

        # Get all non-completed experiments
        incomplete = []
        for short_name, layer, epochs in ModelConfig.get_experiments():
            if not check_training_done(short_name, layer, epochs):
                full_name = next(
                    name
                    for name in ModelConfig.MODELS
                    if name.split("/")[-1] == short_name
                )
                incomplete.append((full_name, layer, epochs))

        # If no incomplete experiments, we're done!
        if not incomplete:
            print("\nAll experiments are complete! Shutting down.")
            break

        # Print status
        print(f"\nFound {len(incomplete)} incomplete experiments")
        for model, layer, epochs in incomplete:
            print(f"- {model.split('/')[-1]} Layer {layer} ({epochs} epochs)")

        # Launch next experiment if GPUs available
        if check_gpu_available():
            model, layer, epochs = incomplete[0]
            print(f"\nLaunching training for {model.split('/')[-1]} Layer {layer}")
            launch_training(model, layer, epochs)
            print("Waiting 1 hour before next check...")
            time.sleep(3600)  # Wait an hour
        else:
            print("GPUs not available")
            print("Checking again in 30 minutes...")
            time.sleep(1800)  # Wait 30 minutes = 1800 seconds


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nShutting down...")
