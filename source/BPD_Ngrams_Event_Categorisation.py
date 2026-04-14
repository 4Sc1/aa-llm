# Uses OpenAI API to derive MAIN event categories from frequent n-grams.
#
# Input: semicolon-separated CSV files created by your n-gram extraction script:
# - ngrams_top1000_bigrams_de.csv  (columns: ngram; n_entries; occurrences)
# - ngrams_top1000_trigrams_de.csv (columns: ngram; n_entries; occurrences)
#
# Output:
# - ngram_derived_main_categories_raw.txt  (verbatim model output)
# - ngram_derived_main_categories.txt      (clean list, one category per line)
# 
# Manuscript: Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: 
#             Contextual Insights from Large Language Models in Ambulatory Assessment
#              
# Author: David Levi Tekampe
# University of Luxembourg

import os
import pandas as pd
from openai import OpenAI

openai_api_key = "<redacted>"
client = OpenAI(api_key=openai_api_key)

bigrams_path = "<path to ngrams_top1000_bigrams_de.csv>"
trigrams_path = "<path to ngrams_top1000_trigrams_de.csv>"

dest_raw  = "<path to output ngram_derived_main_categories_raw.txt>"
dest_list = "<path to output ngram_derived_main_categories.txt>"

sep = ";"
encoding = "utf-8"

max_ngrams_total = 1000

model = "gpt-3.5-turbo"
temperature = 0.2
max_tokens = 1200
timeout_seconds = 60


def load_ngrams(path: str) -> pd.DataFrame:
    df = pd.read_csv(path, low_memory=False, sep=sep, encoding=encoding)
    required = {"ngram", "n_entries", "occurrences"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"{path} missing columns: {missing}. Found: {list(df.columns)}")
    df["ngram"] = df["ngram"].astype(str).str.strip()
    df = df[df["ngram"] != ""]
    return df


def build_ngram_list(df_bi: pd.DataFrame, df_tri: pd.DataFrame, max_total: int) -> list[str]:
    df = pd.concat([df_bi, df_tri], ignore_index=True)
    df = df.sort_values(["n_entries", "occurrences", "ngram"], ascending=[False, False, True])
    df = df.drop_duplicates(subset=["ngram"], keep="first").head(max_total)
    return df["ngram"].tolist()


def derive_main_categories(ngrams: list[str]) -> str:
    if not ngrams:
        return ""

    prompt = """You are a clinical psychologist assisting with qualitative content structuring for a research study. Your patients were asked to list, for each hour of their day, what the main event in that hour was. Usually, patients would state their activity. Sometimes, they also describe their inner states or events that took place in their environment. You will receive frequent bigrams and trigrams (n-grams) extracted from German free-text event entries after preprocessing (spelling/grammar correction, contraction expansion, lowercasing, lemmatisation, stop-word removal, and restriction to nouns, verbs, and adjectives). I will give you the n-grams and I will ask you to propose a parsimonious set of meaningful MAIN event categories that captures the major themes suggested by these n-grams. Keep the number of categories small and usable for a manuscript (avoid many narrow categories). Add one additional category called "Indeterminate" for ambiguous, mixed, or uncategorisable content. Please ONLY respond with the category names, one per line. Do not provide definitions, subcategories, explanations, or any other text. Now, here are the n-grams (one per line):"""

    try:
        response = client.chat.completions.create(
            model=model,
            messages=[{"role": "system", "content": prompt + "\n" + "\n".join(ngrams)}],
            temperature=temperature,
            max_tokens=max_tokens,
            timeout=timeout_seconds,
        )
        return response.choices[0].message.content.strip()
    except Exception as e:
        print("Error:", e)
        return "ERROR!"


def clean_category_list(raw_text: str) -> list[str]:
    out = []
    for ln in raw_text.splitlines():
        s = ln.strip()
        if not s:
            continue
        s = s.lstrip("-•").strip()

        if len(s) >= 3 and s[0].isdigit() and s[1] in [".", ")"]:
            s = s[2:].strip()
        elif len(s) >= 4 and s[0:2].isdigit() and s[2] in [".", ")"]:
            s = s[3:].strip()
        if s:
            out.append(s)

    seen = set()
    uniq = []
    for c in out:
        cl = c.lower()
        if cl not in seen:
            seen.add(cl)
            uniq.append(c)
    return uniq

df_bi = load_ngrams(bigrams_path)
df_tri = load_ngrams(trigrams_path)
ngrams = build_ngram_list(df_bi, df_tri, max_ngrams_total)

for p in [dest_raw, dest_list]:
    if os.path.exists(p):
        os.remove(p)

raw = derive_main_categories(ngrams)

with open(dest_raw, "w", encoding=encoding) as f:
    f.write(raw)

cats = clean_category_list(raw)

if not any(c.strip().lower() == "indeterminate" for c in cats):
    cats.append("Indeterminate")

with open(dest_list, "w", encoding=encoding) as f:
    for c in cats:
        f.write(c + "\n")

print("Saved:")
print(" -", os.path.abspath(dest_raw))
print(" -", os.path.abspath(dest_list))
