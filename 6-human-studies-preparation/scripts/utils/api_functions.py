"""
API utility functions for Claude completions.
"""

import logging
import re
import sys
from pathlib import Path

from anthropic import Anthropic

# Add setup to path for shared utilities
REPO_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(REPO_ROOT))

from setup.api_utils import get_api_key


def get_anthropic_client(api_key=None):
    """Initialize and return an Anthropic client."""
    if api_key is None:
        api_key = get_api_key("ANTHROPIC_API_KEY")
    return Anthropic(api_key=api_key)


def get_claude_completion(
    system_prompt,
    conversation_history,
    anthropic_client,
    max_tokens=100,
    temperature=0.0,
    model_name="claude-3-7-sonnet-20250219",
):
    """Get completion from Anthropic's Claude model."""
    try:
        messages = []
        for turn in conversation_history:
            if turn["role"] == "user":
                messages.append({"role": "user", "content": turn["content"]})
            elif turn["role"] == "assistant":
                messages.append({"role": "assistant", "content": turn["content"]})

        response = anthropic_client.messages.create(
            model=model_name,
            max_tokens=max_tokens,
            messages=messages,
            system=system_prompt,
            temperature=temperature,
        )

        assistant_response = response.content[0].text

        token_usage = {
            "prompt_tokens": response.usage.input_tokens,
            "completion_tokens": response.usage.output_tokens,
            "total_tokens": response.usage.input_tokens + response.usage.output_tokens,
        }

        return assistant_response, token_usage

    except Exception as e:
        logging.error(f"Error in getting Claude completion: {str(e)}")
        return None, None


def extract_tagged_content(response_str, tags):
    """
    Extract content from specified XML-style tags in a response string.

    Args:
        response_str: The string containing tagged content
        tags: List of tag names to extract (without angle brackets)

    Returns:
        dict: Dictionary with tag names as keys and extracted content as values.
              Returns None for tags that aren't found.

    Example:
        extract_tagged_content(response, ['category', 'description'])
        # Returns {'category': 'Health', 'description': 'Lorem ipsum...'}
    """
    results = {}

    for tag in tags:
        pattern = r"<{0}>(.*?)</{0}>".format(tag)
        match = re.search(pattern, response_str, re.DOTALL)
        results[tag] = match.group(1).strip() if match else None

    return results
