import numpy as np

def calculate_cost(model, prompt_tokens, completion_tokens, batch = False):
    """Calculate the estimated cost in USD based on token usage."""
    # Pricing per 1M tokens (as of February 2025)
    pricing = {
        "gpt-4o-2024-08-06": {
            "prompt": 2.50,  # per 1M
            "completion": 10.00 # per 1M
        }
    }

    if model not in pricing:
        return None

    # Calculate cost
    prompt_cost = (prompt_tokens / 1_000_000) * pricing[model]["prompt"]
    completion_cost = (completion_tokens / 1_000_000) * pricing[model]["completion"]
    total_cost = prompt_cost + completion_cost

    if batch:
        prompt_cost = prompt_cost/2
        completion_cost = completion_cost/2
        total_cost = total_cost/2

    return {
        "prompt_cost": np.round(prompt_cost, 6),
        "completion_cost": np.round(completion_cost, 6),
        "total_cost": np.round(total_cost, 6)
    }
