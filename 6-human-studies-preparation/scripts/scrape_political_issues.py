"""
Scrape YouGov survey results for political support/oppose stances.

Collects survey data from YouGov UK and extracts political party breakdowns
for questions containing "support" and "oppose" in the title.

Output: stimuli/main_studies/inputs/yougov_survey_stances_raw.csv
"""

import re
import time
from pathlib import Path

import numpy as np
import pandas as pd
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import (
    TimeoutException,
    ElementClickInterceptedException,
)

# Output path
SCRIPT_DIR = Path(__file__).parent
OUTPUT_DIR = SCRIPT_DIR.parent / "stimuli" / "main_studies" / "inputs"

# YouGov URL with filter for oppose/support surveys after June 2024
FILTERED_URL = (
    "https://yougov.co.uk/topics/overview/"
    "survey-results(popup:search/oppose;after=2024-06-01;"
    "period=custom;type=surveys)"
)


def setup_driver() -> webdriver.Chrome:
    """Initialize headless Chrome driver."""
    chrome_options = Options()
    chrome_options.add_argument("--headless=new")
    chrome_options.add_argument("--disable-gpu")
    chrome_options.add_argument("--window-size=1920,4000")
    chrome_options.add_argument("--log-level=3")
    return webdriver.Chrome(options=chrome_options)


def collect_survey_links(
    url: str, max_scrolls: int = 100, wait_secs: int = 8
) -> pd.DataFrame:
    """Scroll through YouGov survey listing and collect support/oppose survey links."""
    driver = setup_driver()
    driver.get(url)

    # Wait for first card
    WebDriverWait(driver, 15).until(
        EC.presence_of_element_located(
            (By.CSS_SELECTOR, "a[href*='/survey-results/daily/']")
        )
    )

    # Accept cookies if present
    try:
        WebDriverWait(driver, 4).until(
            EC.element_to_be_clickable(
                (By.XPATH, "//button[contains(@id,'onetrust-accept') or text()='Accept']")
            )
        ).click()
    except TimeoutException:
        pass

    processed, titles, links = set(), [], []
    same_height = 0
    last_height = driver.execute_script("return document.body.scrollHeight")

    for i in range(max_scrolls):
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        time.sleep(0.5)

        try:
            load_btn = driver.find_element(
                By.XPATH, "//button[normalize-space()='Load more']"
            )
            driver.execute_script("arguments[0].scrollIntoView();", load_btn)

            prev_count = len(
                driver.find_elements(
                    By.XPATH, "//a[contains(@href,'/survey-results/daily/')]"
                )
            )

            driver.execute_script("arguments[0].click();", load_btn)

            WebDriverWait(driver, wait_secs).until(
                lambda d: (
                    d.execute_script("return document.body.scrollHeight") > last_height
                    or len(
                        d.find_elements(
                            By.XPATH, "//a[contains(@href,'/survey-results/daily/')]"
                        )
                    )
                    > prev_count
                )
            )
            last_height = driver.execute_script("return document.body.scrollHeight")
            same_height = 0
        except Exception:
            same_height += 1

        # Harvest all cards in DOM
        cards = driver.find_elements(
            By.XPATH, "//a[contains(@href,'/survey-results/daily/')]"
        )
        for card in cards:
            try:
                href = card.get_attribute("href")
                if href in processed:
                    continue
                processed.add(href)

                title = card.find_element(By.TAG_NAME, "h2").text
                if "support" in title.lower() and "oppose" in title.lower():
                    links.append(href)
                    titles.append(title)
            except Exception:
                continue

        print(f"Scroll {i+1}/{max_scrolls}: collected {len(links)} items")

        if same_height >= 4:
            print("No further content detected – finishing.")
            break

    driver.quit()
    print(f"\nWebDriver closed. Found {len(links)} support/oppose questions.")

    return pd.DataFrame({"title": titles, "url": links})


def url2date(url: str) -> str:
    """Extract date from URL."""
    m = re.search(r"/daily/(\d{4})/(\d{2})/(\d{2})/", url)
    return "-".join(m.groups()) if m else ""


def _grid_to_dict(grid_html: str) -> dict:
    """Parse YouGov results grid HTML to dictionary."""
    soup = BeautifulSoup(grid_html, "html.parser")
    labels = [lab.text.strip() for lab in soup.select("label.table-label")]
    cells = [c.text.strip().replace("–", "") for c in soup.select("div.cell")]

    if len(labels) < 2:
        return {}

    n_parties = len(cells) // len(labels)
    parties = cells[:n_parties]

    out = {}
    cursor = n_parties
    for lab in labels[1:]:
        row_vals = cells[cursor : cursor + n_parties]
        cursor += n_parties
        out[lab] = {
            p: int(v) if v.isdigit() else (np.nan if v == "" else v)
            for p, v in zip(parties, row_vals)
        }
    return out


def parse_survey_page(url: str, title: str, wait_secs: int = 12) -> dict:
    """Parse a single YouGov survey page and extract political breakdown."""
    driver = setup_driver()
    driver.get(url)

    # Dismiss cookie overlay
    try:
        WebDriverWait(driver, 5).until(
            EC.element_to_be_clickable(
                (
                    By.XPATH,
                    "//button[@id='onetrust-accept-btn-handler' or contains(text(),'Accept')]"
                    "| //button[contains(@class,'onetrust-close-btn-handler')]",
                )
            )
        ).click()
        time.sleep(0.3)
    except TimeoutException:
        pass

    # Parse header
    soup = BeautifulSoup(driver.page_source, "html.parser")
    header = soup.select_one("div.publishing")

    date_raw = header.select_one("p.publishing-date")
    date_text = re.sub(r"^Conducted\s+", "", date_raw.text).strip()

    info_raw = header.select_one("p.publishing-info")
    m = re.search(r"surveyed\s+([\d,]+)", info_raw.text)
    num_adults = m.group(1).replace(",", "") if m else "Unknown"

    # Click "Politics" tab
    try:
        politics_span = WebDriverWait(driver, wait_secs).until(
            EC.element_to_be_clickable((By.CSS_SELECTOR, "span.daily-label-politics"))
        )
        try:
            politics_span.click()
        except ElementClickInterceptedException:
            driver.execute_script("arguments[0].click();", politics_span)
    except TimeoutException:
        driver.quit()
        return {"date": date_text, "adults_surveyed": num_adults, "results": {}}

    # Wait for grid and parse
    try:
        grid_div = WebDriverWait(driver, wait_secs).until(
            EC.presence_of_element_located(
                (
                    By.CSS_SELECTOR,
                    "div.daily-table-size-5, div.daily-table-size-4, div.daily-table-size-6",
                )
            )
        )
        results_dict = _grid_to_dict(grid_div.get_attribute("outerHTML"))
    except TimeoutException:
        results_dict = {}

    driver.quit()
    return {
        "title": title,
        "url": url,
        "date": date_text,
        "adults_surveyed": num_adults,
        "results": results_dict,
    }


def summarise_support_oppose(results_block: dict) -> dict:
    """Collapse YouGov 5-row results grid to {party: {'support': x, 'oppose': y}}."""
    if not results_block:
        return {}

    support_rows = {"Strongly support", "Somewhat support"}
    oppose_rows = {"Somewhat oppose", "Strongly oppose"}

    parties = next(iter(results_block.values())).keys()
    tmp = {p: {"support": [], "oppose": []} for p in parties}

    def to_number(cell):
        s = str(cell).strip().rstrip("%")
        return int(s) if s.isdigit() else None

    for row, party_dict in results_block.items():
        if row in support_rows:
            bucket = "support"
        elif row in oppose_rows:
            bucket = "oppose"
        else:
            continue

        for party, val in party_dict.items():
            num = to_number(val)
            if num is not None:
                tmp[party][bucket].append(num)

    summary = {}
    for party, buckets in tmp.items():
        summary[party] = {
            "support": sum(buckets["support"]) if buckets["support"] else np.nan,
            "oppose": sum(buckets["oppose"]) if buckets["oppose"] else np.nan,
        }

    return summary


def main():
    """Main function to scrape YouGov surveys and save results."""
    print("Collecting survey links from YouGov...")
    df = collect_survey_links(FILTERED_URL)
    df["date"] = df["url"].apply(url2date)

    print(f"\nParsing {len(df)} survey pages...")
    survey_rows = []
    for i, row in df.iterrows():
        print(f"Parsing {i+1}/{len(df)}: {row['title']}")
        survey_data = parse_survey_page(url=row["url"], title=row["title"])
        survey_data["agg_results_pct"] = summarise_support_oppose(survey_data["results"])
        survey_rows.append(survey_data)

    # Save results
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    output_path = OUTPUT_DIR / "yougov_survey_stances_raw.csv"

    survey_df = pd.DataFrame(survey_rows)
    survey_df.to_csv(output_path, index=False)
    print(f"\nSaved {len(survey_df)} surveys to {output_path}")


if __name__ == "__main__":
    main()
