
# Detect potential personal information (PII) in dataset entries.
# 
# Signals:
# - spaCy NER (large models): PERSON, GPE, LOC, ORG, NORP, DATE, TIME
# - Regex: EMAIL, PHONE, URL, HANDLE (@user), IBAN
# 
# Input columns (scanned if present):
#   - event_original          (DE)
#   - event_corrected_de      (DE)
#   - event_en                (EN)
# 
# Outputs (UTF-8, ';'):
#   - suspect_entities.csv : rows containing any PII with per-column matches
#   - pii_summary.csv      : dataset-level counts per PII type
# 
# Manuscript: Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: 
#             Contextual Insights from Large Language Models in Ambulatory Assessment
#              
# Author: David Levi Tekampe
# University of Luxembourg

import re
import json
from collections import Counter, defaultdict
import os

import pandas as pd
from tqdm import tqdm
import spacy

# --------------------- CONFIG ---------------------
CSV_PATH    = "/Users/davidtekampe/Library/Mobile Documents/com~apple~CloudDocs/PhD/data/BPD/dataset_01_r_na.csv"
OUTPUT_PATH = "/Users/davidtekampe/Library/Mobile Documents/com~apple~CloudDocs/PhD/data/BPD/suspect_entities.csv"
SUMMARY_OUT = "/Users/davidtekampe/Library/Mobile Documents/com~apple~CloudDocs/PhD/data/BPD/pii_summary.csv"
SEP         = ";"
ENCODING    = "utf-8"

# Columns to scan (column_name, language)
COLUMNS = [
    ("event_original",     "de"),
    ("event_corrected_de", "de"),
    ("event_en",           "en"),
]

# NER labels of interest (broad, conservative)
NER_LABELS = {"PERSON", "GPE", "LOC", "ORG", "NORP", "DATE", "TIME"}

# --------------------- MODELS ---------------------
try:
    nlp_de = spacy.load("de_core_news_lg")
except OSError as e:
    raise OSError("Missing spaCy model 'de_core_news_lg'. Install with:\n  python -m spacy download de_core_news_lg") from e
try:
    nlp_en = spacy.load("en_core_web_lg")
except OSError as e:
    raise OSError("Missing spaCy model 'en_core_web_lg'. Install with:\n  python -m spacy download en_core_web_lg") from e

# --------------------- REGEXES ---------------------
EMAIL_RE  = re.compile(r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}", re.I)
PHONE_RE  = re.compile(r"(?:(?:\+|00)\d{1,3}[\s\-]?)?(?:\(?\d{2,4}\)?[\s\-]?)?\d{3,4}[\s\-]?\d{3,4}")
URL_RE    = re.compile(r"(?:https?://\S+|www\.\S+)", re.I)
HANDLE_RE = re.compile(r"(?<!\w)@\w{3,}")  # @username, 3+ chars
IBAN_RE   = re.compile(r"\b[A-Z]{2}\d{2}[A-Z0-9]{10,30}\b")

# --------------------- HELPERS ---------------------
def read_semicolon_csv(path: str, sep: str = ";", encoding: str = "utf-8") -> pd.DataFrame:
    """Robust reader: prefer C engine (fast), fallback without low_memory for Python engine if needed."""
    try:
        return pd.read_csv(path, sep=sep, encoding=encoding, engine="c", low_memory=False)
    except TypeError:
        return pd.read_csv(path, sep=sep, encoding=encoding, engine="c")
    except Exception:
        return pd.read_csv(path, sep=sep, encoding=encoding, engine="python")  # no low_memory here

def _regex_hits(text: str) -> dict[str, list[str]]:
    """Regex-based PII hits."""
    if not isinstance(text, str) or not text.strip():
        return {k: [] for k in ("EMAIL","PHONE","URL","HANDLE","IBAN")}
    hits = {
        "EMAIL":  EMAIL_RE.findall(text),
        "PHONE":  PHONE_RE.findall(text),
        "URL":    URL_RE.findall(text),
        "HANDLE": HANDLE_RE.findall(text),
        "IBAN":   IBAN_RE.findall(text),
    }
    # de-duplicate while preserving order
    for k, vals in hits.items():
        seen, uniq = set(), []
        for v in vals:
            if v not in seen:
                uniq.append(v); seen.add(v)
        hits[k] = uniq
    return hits

def _ner_hits(text: str, lang: str) -> dict[str, list[str]]:
    """NER-based PII hits with spaCy."""
    out = {lab: [] for lab in NER_LABELS}
    if not isinstance(text, str) or not text.strip():
        return out
    nlp = nlp_de if lang == "de" else nlp_en
    doc = nlp(text)
    buckets = defaultdict(list)
    for ent in doc.ents:
        if ent.label_ in NER_LABELS:
            buckets[ent.label_].append(ent.text)
    for lab in NER_LABELS:
        if lab in buckets:
            # de-duplicate
            seen, uniq = set(), []
            for v in buckets[lab]:
                if v not in seen:
                    uniq.append(v); seen.add(v)
            out[lab] = uniq
    return out

def scan_column(series: pd.Series, lang: str) -> pd.DataFrame:
    """Return DataFrame with per-row PII lists (JSON strings) for one column."""
    ner_rows, rx_rows = [], []
    for text in tqdm(series.fillna(""), desc=f"Scanning {lang.upper()}", total=len(series)):
        ner_rows.append(_ner_hits(text, lang))
        rx_rows.append(_regex_hits(text))
    ner_df = pd.DataFrame(ner_rows).add_prefix("NER_")
    rx_df  = pd.DataFrame(rx_rows).add_prefix("RX_")
    # Convert lists to JSON strings for CSV
    for c in ner_df.columns:
        ner_df[c] = ner_df[c].apply(lambda x: json.dumps(x, ensure_ascii=False))
    for c in rx_df.columns:
        rx_df[c] = rx_df[c].apply(lambda x: json.dumps(x, ensure_ascii=False))
    return pd.concat([ner_df, rx_df], axis=1)

def _json_list_len(s: str) -> int:
    try:
        obj = json.loads(s)
        return len(obj) if isinstance(obj, list) else 0
    except Exception:
        return 0

# --------------------- MAIN ---------------------
def main():
    # Read dataset
    df = read_semicolon_csv(CSV_PATH, sep=SEP, encoding=ENCODING)

    # Ensure stable row identifier
    if "entry" not in df.columns:
        df["entry"] = range(1, len(df) + 1)

    # Track present columns
    present = [(col, lang) for col, lang in COLUMNS if col in df.columns]
    if not present:
        raise ValueError(f"No expected text columns found. Available: {list(df.columns)}")

    # Scan and attach results
    results = []
    for col, lang in present:
        col_res = scan_column(df[col], lang)
        col_res.columns = [f"{col}__{c}" for c in col_res.columns]
        results.append(col_res)

    out = pd.concat([df] + results, axis=1)

    # Row-level flags per column & global
    for col, _ in present:
        ner_cols = [c for c in out.columns if c.startswith(f"{col}__NER_")]
        rx_cols  = [c for c in out.columns if c.startswith(f"{col}__RX_")]
        out[f"{col}__has_pii"] = out[ner_cols + rx_cols].apply(
            lambda r: any(_json_list_len(v) > 0 for v in r), axis=1
        )
    out["has_any_pii"] = out[[f"{c}__has_pii" for c, _ in present]].any(axis=1)

    # Filter suspects
    suspect = out[out["has_any_pii"]].copy()

    # Save outputs
    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
    suspect.to_csv(OUTPUT_PATH, index=False, encoding=ENCODING, sep=SEP)
    print(f"Saved suspect entries to: {OUTPUT_PATH} (n={len(suspect)})")

    # Summary counts per type across scanned columns
    summary = Counter()
    for col, _ in present:
        for lab in NER_LABELS:
            c = f"{col}__NER_{lab}"
            if c in out.columns:
                summary[f"{col}:{lab}"] += out[c].apply(_json_list_len).sum()
        for rx in ("EMAIL","PHONE","URL","HANDLE","IBAN"):
            c = f"{col}__RX_{rx}"
            if c in out.columns:
                summary[f"{col}:{rx}"] += out[c].apply(_json_list_len).sum()

    summ_df = pd.DataFrame(sorted(summary.items()), columns=["type", "count"])
    summ_df.to_csv(SUMMARY_OUT, index=False, encoding=ENCODING, sep=SEP)
    print(f"Saved summary to: {SUMMARY_OUT}")

if __name__ == "__main__":
    main()
