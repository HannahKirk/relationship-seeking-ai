"""
Process emotional wellbeing topics for experiment.

1. Filter out sensitive/inappropriate topics
2. Check for duplicates using fuzzy matching
3. Use Claude to generate topic descriptions

Input: stimuli/main_studies/inputs/emotional_wellbeing_topics_raw.csv
Output: stimuli/main_studies/inputs/emotchat_topics.csv
"""

import sys
from pathlib import Path

import pandas as pd
from fuzzywuzzy import fuzz

# Add utils to path
SCRIPT_DIR = Path(__file__).parent
sys.path.insert(0, str(SCRIPT_DIR / "utils"))

from api_functions import (
    get_anthropic_client,
    get_claude_completion,
    extract_tagged_content,
)

# Paths
PROJECT_ROOT = SCRIPT_DIR.parent
INPUT_PATH = (
    PROJECT_ROOT / "stimuli" / "main_studies" / "inputs" / "emotional_wellbeing_topics_raw.csv"
)
OUTPUT_PATH = PROJECT_ROOT / "stimuli" / "main_studies" / "inputs" / "emotchat_topics.csv"

# Topics to filter out
SENSITIVE_FILTER = [
    # Physical health
    "Have you recently been ill and had trouble getting the help that you needed?",
    "Do you have problems with you vision or hearing?",
    "Do you have any allergies which regularly bother you?",
    "Do you have any long-term physical health conditions?",
    "Do you have any unexplained pain or other symptoms?",
    "Are you taking any medication? If so, what for?",
    # Personal
    "Have you had a difficult relationship with your parents, partner or children?",
    "Do you have problems maintaining a healthy sexual relationship with partners?",
    "Have you recently lost a close friend or family member and grieve for them?",
    "Do you feel that others want to dominate or control you?",
    "Have you done things to hurt others that you might regret?",
    "Have you been harassed or bullied at work?",
    "Are you currently unemployed?",
    "Do you have any relationships that feel like a burden to you or any that feel very unequal?",
    # Risky advice
    "Do you think that you are short-tempered, or get angry easily?",
    "Do you feel badly treated by your colleagues or line manager?",
    "Do you have financial problems or debts that worry you?",
    "Are you unhappy with your current weight? Would you like to lose weight?",
    "Do have you ever felt betrayed by others?",
]

OTHER_FILTER = [
    "Are there other health issues that bother you?",
    "Are there other relationship issues that bother you?",
    "Are there other careers issues that bother you?",
    "If yes, do you feel undervalued in your current professional position?",
]

# Claude prompt
SYSTEM_PROMPT = """You are an expert at topic classification. You will receive a survey question and a domain. Give a short description of the topic that will be presented to a user in an experiment interface to select from a list of possible topics. This should still be quite general and should be neutrally framed.
Include the response in correct output tags.
Example:
The question is "Do you find your job generally boring or unsatisfying?" The domain is "Career". Return the description in the correct output tags.
<question></question>
<domain>
Desired Output:
<description>Job satisfaction</description>

Example 2:
The question is "Do you find that you often have "brain fog" or difficulty concentrating?" The domain is "Health". Return the description in the correct output tags.
<title></title>
Desired Output:
<description>Tackling with brain fog</description>
"""


def apply_filters(df: pd.DataFrame) -> pd.DataFrame:
    """Remove sensitive and other filtered topics."""
    initial_count = len(df)

    df = df[~df["question_text"].isin(SENSITIVE_FILTER)]
    df = df[~df["question_text"].isin(OTHER_FILTER)]

    print(f"Removed {initial_count - len(df)} filtered items, {len(df)} remaining")
    return df


def find_duplicates(questions: list, threshold: int = 70) -> list:
    """Find duplicate question pairs using fuzzy matching."""
    duplicate_pairs = []
    for i in range(len(questions)):
        for j in range(i + 1, len(questions)):
            similarity = fuzz.token_sort_ratio(questions[i], questions[j])
            if similarity >= threshold:
                duplicate_pairs.append((i, j, similarity))

    duplicate_pairs.sort(key=lambda x: x[2], reverse=True)
    return duplicate_pairs


def check_duplicates(df: pd.DataFrame, threshold: int = 70):
    """Check for potential duplicates and print them."""
    questions = df["question_text"].tolist()
    duplicate_pairs = find_duplicates(questions, threshold)

    if duplicate_pairs:
        print(
            f"\nFound {len(duplicate_pairs)} potential duplicate pairs (threshold {threshold}%):"
        )
        for i, j, score in duplicate_pairs:
            print(f"  Score {score}%: [{i}] vs [{j}]")
    else:
        print(f"\nNo duplicates found (threshold {threshold}%)")


def generate_descriptions(df: pd.DataFrame, client) -> pd.DataFrame:
    """Use Claude to generate topic descriptions."""
    print("\nGenerating topic descriptions...")
    descriptions = []

    for _, row in df.iterrows():
        user_prompt = f"The question is: {row['question_text']}. The domain is {row['pathway_name']}. Return the description in the correct output tags."
        conversation_history = [{"role": "user", "content": user_prompt}]

        response_str, _ = get_claude_completion(
            SYSTEM_PROMPT, conversation_history, client, temperature=0.0
        )
        results = extract_tagged_content(response_str, ["description"])

        descriptions.append(results["description"])
        print(f"  {row['question_text'][:50]}... -> {results['description']}")

    df = df.copy()
    df["description"] = descriptions
    return df


def main():
    """Main processing pipeline."""
    print(f"Loading data from {INPUT_PATH}")
    df = pd.read_csv(INPUT_PATH)
    print(f"Loaded {len(df)} rows")

    # Step 1: Apply filters
    print("\n--- Applying filters ---")
    df = apply_filters(df)

    # Step 2: Check for duplicates (informational only)
    print("\n--- Checking for duplicates ---")
    check_duplicates(df, threshold=70)

    # Step 3: Initialize Claude client
    print("\n--- Connecting to Claude API ---")
    client = get_anthropic_client()

    # Step 4: Generate descriptions
    df = generate_descriptions(df, client)

    # Print summary
    print(f"\nDomain distribution:")
    print(df["pathway_name"].value_counts())

    # Save output
    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(OUTPUT_PATH, index=False)
    print(f"\nSaved {len(df)} processed items to {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
