"""
Shared API key utilities.

Handles API key retrieval for both:
- Internal AISI use (via proxy)
- External users (direct API keys)
"""

import os

try:
    from aisitools.api_key import get_api_key_for_proxy
    _USE_PROXY = True
except ImportError:
    _USE_PROXY = False


def get_api_key(env_var: str) -> str:
    """
    Get API key from environment variable.

    Uses AISI proxy if aisitools is installed, otherwise uses key directly.

    Args:
        env_var: Name of environment variable (e.g., "OPENAI_API_KEY")

    Returns:
        API key string

    Raises:
        ValueError: If environment variable is not set
    """
    key = os.environ.get(env_var)
    if not key:
        raise ValueError(
            f"{env_var} not set. Export it with: export {env_var}=your-key"
        )
    if _USE_PROXY:
        return get_api_key_for_proxy(key)
    return key
