"""
Process scraped YouGov political survey data.

1. Remove duplicate/similar survey titles using fuzzy matching
2. Filter out sensitive topics
3. Use Claude to classify topics into categories
4. Use Claude to generate policy statements

Input: stimuli/main_studies/inputs/yougov_survey_stances_raw.csv
Output: stimuli/main_studies/inputs/yougov_survey_stances_processed.csv
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
INPUT_PATH = PROJECT_ROOT / "stimuli" / "main_studies" / "inputs" / "yougov_survey_stances_raw.csv"
OUTPUT_PATH = PROJECT_ROOT / "stimuli" / "main_studies" / "inputs" / "yougov_survey_stances_processed.csv"

# Topics to filter out
SENSITIVE_FILTER = [
    "Would you support or oppose a ban on pornography depicting strangulation?",
    "Do you support or oppose a ceasefire between Israel and Hamas at this time?",
    "Would you support or oppose judges taking into consideration when sentencing whether an offender is from an ethnic, religious or cultural minority?",
    "Do you support or oppose the UK suspending some arms exports to Israel?",
    "Would you support or oppose increasing the amount of time that migrants have to live in the UK before they are able to apply for British citizenship?",
    "Would you support or oppose increasing the amount of time that migrants have to live in the UK before they are legally entitled to stay on a permanent basis?",
    "Would you support or oppose a move to increase the number of deportations of illegal immigrants from the UK?",
    "Currently a crime can be considered a \"hate crime\" if committed against someone because of their race, religion, sexual orientation, disability or transgender identity. Would you support or oppose including crimes committed against someone because of their sex or gender to this list?",
    "From what you've read and heard, do you support or oppose the Conservative Party's plan to send some migrants to Rwanda?",
]

OTHER_FILTER = [
    "Imagine a medical procedure exists that completely separates a person's work and personal memories, so they have no recollection of their job while outside of work and no personal memories while at work. Would you support or oppose allowing people to voluntarily undergo this procedure?",
    "From everything you have seen and heard about the issue, do you support or oppose the government's proposals to give sovereignty of the Chagos Islands to Mauritius?",
    "Would you support or oppose J.K. Rowling being given a peerage?",
    "Would you support or oppose having an additional bank holiday in May of this year to mark the 80th anniversary of VE Day (the end of World War 2 in Europe)?",
]

# Claude prompts
CATEGORY_SYSTEM_PROMPT = """You are an expert at topic classification. You will receive the title of an opinion poll from YouGov. You should return two pieces of information.
1. Classify the topic into simple category e.g., Health, Legal, Defence etc.
2. Give a short description of the topic that will be presented to a user in an experiment interface to select from a list of possible topics. This should still be quite general and should be neutrally framed, not in favour or against the policy.
Include the pieces of information in correct output tags.
Example:
<title>"Would you support or oppose the NHS increasing its use of private sector health providers to bring down waiting times, with care remaining free at the point‑of-use?"</title>
Desired Output:
<category>Health</category>
<description>Privatisation of the NHS</description>

Example 2:
<title>Would you support or oppose the construction of a nuclear power station in your local area?</title>
Desired Output:
<category>Energy</category>
<description>Nuclear power plants</description>
"""

STATEMENT_SYSTEM_PROMPT = """You are an expert at political stance generation. You will receive the title of an opinion poll from YouGov. Your job is to convert the poll title into a political issue statement.:
Each stance should:
- Be framed as a clear position (starting with "The U.K. SHOULD" )
- Be written in clear, concise language a voter would understand.
- Capture the contextual details of the poll title.

EXAMPLE 1:
<title>Would you support or oppose the NHS increasing its use of private sector health providers to bring down waiting times, with care remaining free at the point‑of-use?</title>

<statement>The U.K. SHOULD limit the NHS's use of private sector health providers to bring down waiting times, with care remaining free at the point‑of-use.</statement>

EXAMPLE 2:
<title>Do you support or oppose winter fuel payments to pensioners being means‑tested, so they only go to those on pension credit or other means-tested benefits?</title>

<statement>The U.K. SHOULD introduce means-testing for winter fuel payments to pensioners, so they only go to those on pension credit or other means-tested benefits.</statement>

EXAMPLE 3:
<title>Would you support or oppose the NHS providing smart watches to allow people with certain conditions (such as high blood pressure, cancer, diabetes) to measure their vital signs?</title>

<statement>The U.K. SHOULD provide NHS-funded wearable devices to allow people with certain conditions (such as high blood pressure, cancer, diabetes) to measure their vital signs.</statement>

EXAMPLE 4:
<title>Do you support or oppose undergraduate tuition fees in England rising £285 per year to £9,535?</title>

<statement>The U.K. SHOULD allow universities to increase annual tuition fees in line with inflation (rising £285 per year to £9,535).</statement>
"""


def find_duplicates(titles: list, threshold: int = 70) -> list:
    """Find duplicate title pairs using fuzzy matching."""
    duplicate_pairs = []
    for i in range(len(titles)):
        for j in range(i + 1, len(titles)):
            similarity = fuzz.token_sort_ratio(titles[i], titles[j])
            if similarity >= threshold:
                duplicate_pairs.append((i, j, similarity))

    duplicate_pairs.sort(key=lambda x: x[2], reverse=True)
    return duplicate_pairs


def remove_duplicates(df: pd.DataFrame, threshold: int = 70) -> pd.DataFrame:
    """Remove duplicate titles using fuzzy matching."""
    titles = df["title"].tolist()
    duplicate_pairs = find_duplicates(titles, threshold)

    print(f"Found {len(duplicate_pairs)} potential duplicate pairs (threshold {threshold}%)")
    for i, j, score in duplicate_pairs:
        print(f"  Score {score}%: [{i}] vs [{j}]")

    # Remove second title from each pair
    indices_to_drop = set()
    for i, j, score in duplicate_pairs:
        indices_to_drop.add(df.index[j])

    df = df.drop(indices_to_drop)
    print(f"Remaining after removing duplicates: {len(df)}")
    return df


def apply_filters(df: pd.DataFrame) -> pd.DataFrame:
    """Remove sensitive and other filtered topics."""
    initial_count = len(df)

    for title in SENSITIVE_FILTER + OTHER_FILTER:
        df = df[df["title"] != title]

    df = df.drop_duplicates(subset=["title"], keep="first")

    print(f"Removed {initial_count - len(df)} filtered items, {len(df)} remaining")
    return df


def classify_topics(df: pd.DataFrame, client) -> pd.DataFrame:
    """Use Claude to classify topics into categories."""
    print("\nClassifying topics...")
    categories = []
    descriptions = []

    for i, row in df.iterrows():
        user_prompt = f"The topic title is: {row['title']}. Return the topic category and description in the correct output tags."
        conversation_history = [{"role": "user", "content": user_prompt}]

        response_str, _ = get_claude_completion(
            CATEGORY_SYSTEM_PROMPT, conversation_history, client, temperature=0.0
        )
        results = extract_tagged_content(response_str, ["category", "description"])

        categories.append(results["category"])
        descriptions.append(results["description"])
        print(f"  {row['title'][:60]}... -> {results['category']}")

    df["category"] = categories
    df["description"] = descriptions
    return df


def generate_statements(df: pd.DataFrame, client) -> pd.DataFrame:
    """Use Claude to generate policy statements."""
    print("\nGenerating policy statements...")
    statements = []

    for i, row in df.iterrows():
        user_prompt = f"The topic title is: {row['title']}. Return policy issue statements in the correct output tag."
        conversation_history = [{"role": "user", "content": user_prompt}]

        response_str, _ = get_claude_completion(
            STATEMENT_SYSTEM_PROMPT, conversation_history, client, temperature=0.0
        )
        results = extract_tagged_content(response_str, ["statement"])

        statements.append(results["statement"])
        print(f"  {results['statement'][:80]}...")

    df["statement"] = statements
    return df


def main():
    """Main processing pipeline."""
    print(f"Loading data from {INPUT_PATH}")
    df = pd.read_csv(INPUT_PATH)
    print(f"Loaded {len(df)} rows")

    # Step 1: Remove duplicates
    print("\n--- Removing duplicates ---")
    df = remove_duplicates(df, threshold=70)

    # Step 2: Apply filters
    print("\n--- Applying filters ---")
    df = apply_filters(df)

    # Step 3: Initialize Claude client
    print("\n--- Connecting to Claude API ---")
    client = get_anthropic_client()

    # Step 4: Classify topics
    df = classify_topics(df, client)

    # Step 5: Generate statements
    df = generate_statements(df, client)

    # Save output
    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(OUTPUT_PATH, index=False)
    print(f"\nSaved {len(df)} processed items to {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
