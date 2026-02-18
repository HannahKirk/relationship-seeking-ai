"""
Test script to verify API keys are working.

Run from repo root:
    python setup/test_api_keys.py
"""

from anthropic import Anthropic
from openai import OpenAI

from api_utils import get_api_key

# OpenAI Test
print("Testing OpenAI API...")
client = OpenAI(api_key=get_api_key("OPENAI_API_KEY"))

response = client.chat.completions.create(
    model="gpt-4o-2024-08-06",
    messages=[{"role": "user", "content": "Hello gpt-4o!"}],
    temperature=0,
)
print(f"OpenAI Response: {response}\n")

# Anthropic Test
print("Testing Anthropic API...")
client = Anthropic(api_key=get_api_key("ANTHROPIC_API_KEY"))

response = client.messages.create(
    model="claude-3-5-sonnet-20241022",
    max_tokens=1024,
    messages=[{"role": "user", "content": "Hello Claude!"}],
    temperature=0,
)
print(f"Anthropic Response: {response}\n")

# Get Anthropic model list
models = client.models.list(limit=20)
print("Anthropic Models:")
print(models)
