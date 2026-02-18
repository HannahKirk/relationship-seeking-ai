MODELS_TO_INCLUDE = {
    # Anthropic (11 models)
    "anthropic/claude-3-opus",
    "anthropic/claude-3-haiku",
    "anthropic/claude-3.5-sonnet",
    "anthropic/claude-3.5-haiku",
    "anthropic/claude-3.7-sonnet",
    "anthropic/claude-opus-4",
    "anthropic/claude-opus-4.1",
    "anthropic/claude-sonnet-4",
    "anthropic/claude-sonnet-4.5",
    "anthropic/claude-haiku-4.5",
    "anthropic/claude-opus-4.5",
    # OpenAI (15 models)
    "openai/gpt-3.5-turbo",
    "openai/gpt-4",
    "openai/gpt-4-turbo-preview",
    "openai/gpt-4-turbo",
    "openai/chatgpt-4o-latest",
    "openai/gpt-4o",
    "openai/gpt-4o-2024-11-20",
    "openai/gpt-4.1",
    "openai/gpt-4.1-nano",
    "openai/gpt-4.1-mini",
    "openai/gpt-oss-20b",
    "openai/gpt-5-chat",
    "openai/gpt-5.1-chat",
    "openai/gpt-5.1",
    "openai/gpt-5-mini",
    # Google (10 models)
    "google/gemma-2-9b-it",
    "google/gemma-2-27b-it",
    "google/gemma-3-27b-it",
    "google/gemma-3-12b-it",
    "google/gemma-3n-e4b-it",
    "google/gemini-2.0-flash-001",
    "google/gemini-2.5-pro-preview",
    "google/gemini-2.5-flash",
    "google/gemini-2.5-flash-lite",
    "google/gemini-3-pro-preview",
    # Meta-Llama (10 models)
    "meta-llama/llama-3-8b-instruct",
    "meta-llama/llama-3-70b-instruct",
    "meta-llama/llama-3.1-8b-instruct",
    "meta-llama/llama-3.1-70b-instruct",
    "meta-llama/llama-3.1-405b-instruct",
    "meta-llama/llama-3.2-1b-instruct",
    "meta-llama/llama-3.2-3b-instruct:free",
    "meta-llama/llama-3.3-70b-instruct:free",
    "meta-llama/llama-4-scout",
    "meta-llama/llama-4-maverick",
    # Mistral (8 models)
    "mistralai/mistral-7b-instruct",
    "mistralai/mixtral-8x7b-instruct",
    "mistralai/mixtral-8x22b-instruct",
    "mistralai/mistral-nemo",
    "mistralai/mistral-large-2407",
    "mistralai/mistral-saba",
    "mistralai/mistral-small-3.2-24b-instruct",
    "mistralai/mistral-medium-3.1",
    # Qwen (8 models)
    "qwen/qwen-2.5-72b-instruct",
    "qwen/qwen-2.5-vl-7b-instruct",
    "qwen/qwen-turbo",
    "qwen/qwq-32b",
    "qwen/qwen3-30b-a3b",
    "qwen/qwen3-30b-a3b-instruct-2507",
    "qwen/qwen-plus-2025-07-28",
    "qwen/qwen3-next-80b-a3b-instruct",
    # DeepSeek (4 models)
    "deepseek/deepseek-chat",
    "deepseek/deepseek-chat-v3-0324",
    "deepseek/deepseek-chat-v3.1",
    "deepseek/deepseek-v3.2-exp",
    # X-AI (5 models)
    "x-ai/grok-3-beta",
    "x-ai/grok-3",
    "x-ai/grok-4",
    "x-ai/grok-4-fast",
    "x-ai/grok-4.1-fast",
    # Microsoft (5 models)
    "microsoft/wizardlm-2-8x22b",
    "microsoft/phi-3-medium-128k-instruct",
    "microsoft/phi-3.5-mini-128k-instruct",
    "microsoft/phi-4",
    "microsoft/phi-4-multimodal-instruct",
    # Cohere (3 models)
    "cohere/command-a",
    "cohere/command-r7b-12-2024",
    "cohere/command-r-08-2024",
    # Nvidia (3 models)
    "nvidia/llama-3.1-nemotron-70b-instruct",
    "nvidia/nemotron-nano-9b-v2:free",
    "nvidia/nemotron-nano-12b-v2-vl:free",
    # MoonshotAI (3 models)
    "moonshotai/kimi-dev-72b",
    "moonshotai/kimi-k2",
    "moonshotai/kimi-k2-0905",
    # Baidu (2 models)
    "baidu/ernie-4.5-300b-a47b",
    "baidu/ernie-4.5-21b-a3b",
    # Z-AI (2 models)
    "z-ai/glm-4.5",
    "z-ai/glm-4.6",
    # AllenAI (2 models)
    "allenai/olmo-2-0325-32b-instruct",
    "allenai/olmo-3-7b-instruct",
    # Amazon (2 models)
    "amazon/nova-premier-v1",
    "amazon/nova-pro-v1",
    # Perplexity (2 models)
    "perplexity/sonar",
    "perplexity/sonar-pro",
    # AI21 (1 model)
    "ai21/jamba-large-1.7",
    # Inflection (1 model)
    "inflection/inflection-3-pi",
    # Liquid (1 model)
    "liquid/lfm2-8b-a1b",
    # Meituan (1 model)
    "meituan/longcat-flash-chat",
    # Raifle (1 model)
    "raifle/sorcererlm-8x22b",
    # StepFun-AI (1 model)
    "stepfun-ai/step3",
    # Tencent (1 model)
    "tencent/hunyuan-a13b-instruct",
}
