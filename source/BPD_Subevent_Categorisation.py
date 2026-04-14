# Uses OpenAI API to derive SUBCATEGORIES (per MAIN category) from EMA free-text event entries.
#
# Input: semicolon-separated CSV file
# - columns:
#   - event_category (MAIN category label)
#   - event_en (preferred) OR event_corrected_de (fallback)
#
# Output:
# - event_subcategories_raw.txt   (verbatim model output; audit trail)
# - event_subcategories.csv       (MAIN category -> subcategory list; one row per MAIN category)
# 
# Manuscript: Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: 
#             Contextual Insights from Large Language Models in Ambulatory Assessment
#              
# Author: David Levi Tekampe
# University of Luxembourg

import pandas as pd
import os
from openai import OpenAI

openai_api_key = "<redacted>"
client = OpenAI(api_key=openai_api_key)

file_path = "<path to input dataset_01_r_na_complete.csv>"

MAIN_CATEGORY_COL = "event_category"
TEXT_COL_PRIMARY = "event_en"
TEXT_COL_FALLBACK = "event_corrected_de"

EXCLUDE_CATEGORIES = {"Indeterminate", "None", "", None}

MAIN_CATEGORIES = [
    "Daily Routines and Household Activities",
    "Leisure and Recreation",
    "Social and Personal Relationships",
    "Work and Professional Engagements",
    "Health and Well-being",
]

RANDOM_SEED = 42
SAMPLE_PER_CATEGORY = 250
MAX_CHARS_PER_ENTRY = 180

model = "gpt-3.5-turbo"
temperature = 0.2
max_tokens = 1400
timeout_seconds = 60

dest_raw = "<path to output event_subcategories_raw.txt>"
dest_csv = "<path to output event_subcategories.csv>"

sep = ";"
encoding = "utf-8"

def clean_entry(x):
    s = str(x).strip().replace("\t", " ").replace("\n", " ")
    s = " ".join(s.split())
    if len(s) > MAX_CHARS_PER_ENTRY:
        s = s[:MAX_CHARS_PER_ENTRY].rstrip() + "…"
    return s

def derive_subcategories(main_category, examples):
    """
    Returns raw model output, expected as:
    - One subcategory per line
    - No extra text
    """
    if not examples:
        return ""

    system_prompt = """You are a clinical psychologist. Your patients were asked to list for each hour of their day what the main event in that hour was. Usually, patients would state their activity. When in doubt, always interpret a statement as a description of what they were doing. Sometimes, they also describe their inner states or events that took place in their environment. I will give you a MAIN CATEGORY and short event descriptions that have already been assigned to this MAIN category, and I will ask you to propose a parsimonious set of meaningful SUBCATEGORIES within this MAIN category. You can assume the following MAIN CATEGORIES and their meanings: (1) Daily Routines and Household Activities: Activities integral to daily living and maintaining a household; typically in and around the home, involving personal and domestic spaces; regular and necessary tasks for personal and home care. (2) Leisure and Recreation: Activities undertaken for enjoyment, relaxation, or personal fulfilment; ranging from home to outdoor settings, cultural venues, and community spaces; personal enjoyment and relaxation, away from work and routine responsibilities. (3) Social and Personal Relationships: Interactions and activities centred around building and maintaining relationships; occurring in personal spaces and/or public and community settings; emotional, social, and supportive exchanges with others. (4) Work and Professional Engagements: Activities related to one’s profession, job responsibilities, education, and career development; typically in workplaces, educational institutions, or other professional settings; includes specific tasks or projects, work-related discussions/emails/meetings, professional travel/events, and interactions pertaining to the work environment. (5) Health and Well-being: Activities and experiences related to physical, mental, and emotional health; including healthcare settings, therapy, personal spaces, and fitness contexts; health maintenance, medical care, and personal well-being practices. (6) Indeterminate: Use for entries that are too vague, ambiguous, mixed, or otherwise not classifiable within the above categories. Your task is to propose a parsimonious set of meaningful SUBCATEGORIES within the provided MAIN category based on the event descriptions I give you. Keep the number of subcategories small and usable for a manuscript (avoid many narrow categories). Use clear, concrete labels in Title Case. If needed, include an “Other/Unclear” subcategory for vague, mixed, or difficult-to-classify entries. Do not infer identities or sensitive attributes; code only what is described. Now, please list the subcategory names. Please ONLY respond with the subcategory names, one per line. Nothing else."""

    try:
        response = client.chat.completions.create(
            model=model,
            messages=[
                {"role": "system", "content": system_prompt},
                {
                    "role": "user",
                    "content": (
                        f"MAIN CATEGORY: {main_category}\n"
                        "Event descriptions (one per line):\n"
                        + "\n".join(examples)
                    ),
                },
            ],
            temperature=temperature,
            max_tokens=max_tokens,
            timeout=timeout_seconds,
        )
        return response.choices[0].message.content.strip()
    except Exception as e:
        print("Error: ", e)
        return "ERROR!"



def clean_subcategory_list(raw_text):
    out = []
    for ln in raw_text.splitlines():
        s = ln.strip()
        if not s:
            continue
        s = s.lstrip("-•").strip()

        # strip leading numbering like "1." or "1)"
        if len(s) >= 3 and s[0].isdigit() and s[1] in [".", ")"]:
            s = s[2:].strip()
        elif len(s) >= 4 and s[0].isdigit() and s[1].isdigit() and s[2] in [".", ")"]:
            s = s[3:].strip()

        if s:
            out.append(s)

    seen = set()
    unique = []
    for c in out:
        cl = c.lower()
        if cl not in seen:
            seen.add(cl)
            unique.append(c)

    return unique

df = pd.read_csv(file_path, low_memory=False, sep=sep, encoding=encoding)

text_col = TEXT_COL_PRIMARY if TEXT_COL_PRIMARY in df.columns else TEXT_COL_FALLBACK
if text_col not in df.columns:
    raise ValueError(f"Neither '{TEXT_COL_PRIMARY}' nor '{TEXT_COL_FALLBACK}' exists in the dataset.")

if MAIN_CATEGORY_COL not in df.columns:
    raise ValueError(f"Missing '{MAIN_CATEGORY_COL}' column in the dataset.")

if MAIN_CATEGORIES is None:
    cats = sorted(set(df[MAIN_CATEGORY_COL].dropna().astype(str).str.strip()))
    cats = [c for c in cats if c not in EXCLUDE_CATEGORIES]
else:
    cats = [c for c in MAIN_CATEGORIES if c not in EXCLUDE_CATEGORIES]

if os.path.exists(dest_raw):
    os.remove(dest_raw)
if os.path.exists(dest_csv):
    os.remove(dest_csv)

rows_out = []

for main_cat in cats:
    sub = df[df[MAIN_CATEGORY_COL].astype(str).str.strip() == main_cat].copy()

    sub[text_col] = sub[text_col].astype(str)
    sub["__txt"] = sub[text_col].apply(clean_entry)
    sub = sub[sub["__txt"].str.len() > 0]

    if len(sub) == 0:
        rows_out.append({"main_category": main_cat, "subcategories": ""})
        continue

    n = min(SAMPLE_PER_CATEGORY, len(sub))
    examples = sub.sample(n=n, random_state=RANDOM_SEED)["__txt"].tolist()

    raw = derive_subcategories(main_cat, examples)

    with open(dest_raw, "a", encoding=encoding) as f:
        f.write("\n" + "=" * 80 + "\n")
        f.write(f"MAIN CATEGORY: {main_cat}\n")
        f.write("=" * 80 + "\n")
        f.write(raw + "\n")

    subs = clean_subcategory_list(raw)

    rows_out.append({"main_category": main_cat, "subcategories": " | ".join(subs)})

out_df = pd.DataFrame(rows_out)
out_df.to_csv(dest_csv, index=False, sep=sep, encoding=encoding)

print("Saved:")
print(" -", os.path.abspath(dest_raw))
print(" -", os.path.abspath(dest_csv))