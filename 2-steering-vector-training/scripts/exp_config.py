class ModelConfig:
    # Layer configurations for different model sizes
    LAYER_CONFIGS = {
        "32": {
            "run_order": [14, 13, 15, 11, 17, 5, 23],
            "layers": [5, 11, 13, 14, 15, 17, 23],
        },
        "80": {
            "run_order": [21, 31, 33, 35, 27, 29, 41],
            "layers": [21, 31, 33, 35, 27, 29, 41],
        },
    }

    # Mapping of model names to their layer types
    MODELS = {
        "meta-llama/Llama-3.1-70B-Instruct": "80",
        "meta-llama/Llama-3.1-8B-Instruct": "32",
    }

    @classmethod
    def get_experiments(cls):
        """Returns list of (short_name, layer, train_epochs) for all experiments"""
        experiments = []
        for model_name, layer_type in cls.MODELS.items():
            short_name = model_name.split("/")[-1]
            run_order = cls.LAYER_CONFIGS[layer_type]["run_order"]

            for i, layer in enumerate(run_order):
                train_epochs = 20
                experiments.append((short_name, layer, train_epochs))

        return experiments


if __name__ == "__main__":
    # Print all experiments
    for model, layer, epochs in ModelConfig.get_experiments():
        print(f"{model} Layer {layer}: {epochs} epochs")


# Constants
N_TEST_PROMPTS = 245
N_MULTIPLIERS = 21
N_PAIRWISE_MULTIPLIERS = 105
N_RATINGS = N_TEST_PROMPTS * N_MULTIPLIERS
N_PAIRWISE_RANKINGS = N_TEST_PROMPTS * N_PAIRWISE_MULTIPLIERS
