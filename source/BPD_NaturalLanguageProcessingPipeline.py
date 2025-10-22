
# Bilingual (EN/DE) preprocessing and n-gram extraction
# 
# Pipeline (translation omitted):
# 1) Spelling/grammar correction (LanguageTool; en-GB, de-DE)
# 2) Contraction expansion (BEFORE punctuation filtering)
# 3) Lowercasing
# 4) Tokenisation + Lemmatization (spaCy: en_core_web_lg, de_core_news_lg)
# 5) Stop-word removal (spaCy)
# 6) POS filter: keep only NOUN, VERB, ADJ
# 7) N-gram extraction (bigrams/trigrams) from lemma sequences
# 8) Aggregate and export: rank by #unique entries containing the n-gram (old-script behaviour),
#    also include total occurrence counts for transparency.
# 
# Outputs (UTF-8, ';') under OUTPUT_DIR:
#   - preprocessed_events.csv
#   - ngrams_top1000_bigrams_en.csv
#   - ngrams_top1000_trigrams_en.csv
#   - ngrams_top1000_bigrams_de.csv
#   - ngrams_top1000_trigrams_de.csv
#   - ngrams_top1000_bigrams_combined.csv
#   - ngrams_top1000_trigrams_combined.csv
# 
# Dependencies:
#   pip install spacy language-tool-python contractions pandas tqdm
#   python -m spacy download en_core_web_lg
#   python -m spacy download de_core_news_lg
#
# Author: David Levi Tekampe
# University of Luxembourg

import os
import re
from collections import Counter, defaultdict
from itertools import islice
import pandas as pd
from tqdm import tqdm

import spacy
import contractions

# --------------------- config ---------------------
INPUT_CSV = "<path to input CSV file>"
OUTPUT_DIR = "<path to output directory>"
CSV_DELIMITER = ";"
ENCODING = "utf-8"

# column mapping
COL_EN = "event_en"
COL_DE = "event_corrected_de"

# toggle language_tool
ENABLE_GRAMMAR = True

TOP_K = 1000
BATCH_SIZE = 1000
ALLOWED_POS = {"NOUN", "VERB", "ADJ"}

# spaCy models
try:
    nlp_en = spacy.load("en_core_web_lg")
except OSError as e:
    raise OSError(
        "spaCy model 'en_core_web_lg' not found. Install with:\n"
        "python -m spacy download en_core_web_lg"
    ) from e

try:
    nlp_de = spacy.load("de_core_news_lg")
except OSError as e:
    raise OSError(
        "spaCy model 'de_core_news_lg' not found. Install with:\n"
        "python -m spacy download de_core_news_lg"
    ) from e

# language_tool (optional)
tool_en = tool_de = None
if ENABLE_GRAMMAR:
    try:
        import language_tool_python
        tool_en = language_tool_python.LanguageToolPublicAPI("en-GB")
        tool_de = language_tool_python.LanguageToolPublicAPI("de-DE")
    except Exception:
        tool_en = tool_de = None

def grammar_and_spelling(text: str, lang: str) -> str:
    if not isinstance(text, str) or not text.strip():
        return ""
    if not ENABLE_GRAMMAR:
        return text
    tool = tool_en if lang == "en" else tool_de
    if tool is None:
        return text
    try:
        return tool.correct(text)
    except Exception:
        return text

# contraction expansion
_DE_CONTRACTIONS = {
    r"\bam\b": "an dem",
    r"\bim\b": "in dem",
    r"\bins\b": "in das",
    r"\bbeim\b": "bei dem",
    r"\bvom\b": "von dem",
    r"\bzum\b": "zu dem",
    r"\bzur\b": "zu der",
}

# --------------------- helpers ---------------------
def expand_contractions(text: str, lang: str) -> str:
    if not isinstance(text, str) or not text.strip():
        return ""
    if lang == "en":
        return contractions.fix(text)
    elif lang == "de":
        out = text
        for pat, repl in _DE_CONTRACTIONS.items():
            out = re.sub(pat, repl, out, flags=re.IGNORECASE)
        return out
    return text

def normalise_to_lemmas(text: str, lang: str) -> list[str]:
    if not isinstance(text, str) or not text.strip():
        return []

    txt = grammar_and_spelling(text, lang=lang)
    txt = expand_contractions(txt, lang=lang)
    txt = txt.lower()

    nlp = nlp_en if lang == "en" else nlp_de
    doc = nlp(txt)

    lemmas = []
    for t in doc:
        if not t.is_alpha:
            continue
        if t.is_space or t.is_stop:
            continue
        if t.pos_ not in ALLOWED_POS:
            continue
        lemma = t.lemma_.strip()
        if lemma:
            lemmas.append(lemma)
    return lemmas

def ngrams(tokens: list[str], n: int) -> list[str]:
    return [" ".join(tokens[i:i+n]) for i in range(len(tokens) - n + 1)]

def rank_like_old_script(entry_sets: dict[str, set], counts: Counter, k: int) -> pd.DataFrame:
    rows = []
    for ng in counts.keys() | entry_sets.keys():
        n_entries = len(entry_sets.get(ng, set()))
        occ = counts.get(ng, 0)
        rows.append((ng, n_entries, occ))
    df = pd.DataFrame(rows, columns=["ngram", "n_entries", "occurrences"])
    df = df.sort_values(by=["n_entries", "occurrences", "ngram"], ascending=[False, False, True])
    return df.head(k)

def save_csv(df: pd.DataFrame, name: str):
    path = os.path.join(OUTPUT_DIR, name)
    df.to_csv(path, index=False, encoding=ENCODING, sep=CSV_DELIMITER)
    print(f"Saved: {path}")

# --------------------- main ---------------------
def main():
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    df = pd.read_csv(INPUT_CSV, delimiter=CSV_DELIMITER, encoding=ENCODING, low_memory=False)

    has_en = COL_EN in df.columns
    has_de = COL_DE in df.columns
    if not (has_en or has_de):
        raise ValueError(f"No language columns found. Available: {list(df.columns)}")

    entry_ids = df.get("entry", pd.RangeIndex(start=1, stop=len(df)+1))

    if has_en:
        tqdm.pandas(desc="EN: preprocessing")
        df["lemmas_en"] = df[COL_EN].progress_apply(lambda s: normalise_to_lemmas(s, "en"))
        df["processed_en"] = df["lemmas_en"].apply(lambda toks: " ".join(toks))
    if has_de:
        tqdm.pandas(desc="DE: preprocessing")
        df["lemmas_de"] = df[COL_DE].progress_apply(lambda s: normalise_to_lemmas(s, "de"))
        df["processed_de"] = df["lemmas_de"].apply(lambda toks: " ".join(toks))

    save_csv(df, "preprocessed_events.csv")

    def tally_lang(tokens_series: pd.Series):
        big_c = Counter(); tri_c = Counter()
        big_entries = defaultdict(set); tri_entries = defaultdict(set)
        for toks, eid in zip(tokens_series, entry_ids):
            if not isinstance(toks, list):
                continue
            bi = ngrams(toks, 2); tri = ngrams(toks, 3)
            big_c.update(bi); tri_c.update(tri)
            # unique-entry membership per n-gram
            for ng in set(bi): big_entries[ng].add(eid)
            for ng in set(tri): tri_entries[ng].add(eid)
        return big_c, tri_c, big_entries, tri_entries

    big_en = tri_en = big_en_entries = tri_en_entries = None
    big_de = tri_de = big_de_entries = tri_de_entries = None

    if has_en:
        big_en, tri_en, big_en_entries, tri_en_entries = tally_lang(df["lemmas_en"])
        df_top_bi_en  = rank_like_old_script(big_en_entries,  big_en,  TOP_K)
        df_top_tri_en = rank_like_old_script(tri_en_entries, tri_en, TOP_K)
        save_csv(df_top_bi_en,  "ngrams_top1000_bigrams_en.csv")
        save_csv(df_top_tri_en, "ngrams_top1000_trigrams_en.csv")

    if has_de:
        big_de, tri_de, big_de_entries, tri_de_entries = tally_lang(df["lemmas_de"])
        df_top_bi_de  = rank_like_old_script(big_de_entries,  big_de,  TOP_K)
        df_top_tri_de = rank_like_old_script(tri_de_entries, tri_de, TOP_K)
        save_csv(df_top_bi_de,  "ngrams_top1000_bigrams_de.csv")
        save_csv(df_top_tri_de, "ngrams_top1000_trigrams_de.csv")

    big_all = Counter()
    tri_all = Counter()
    big_all_entries = defaultdict(set)
    tri_all_entries = defaultdict(set)

    if has_en:
        big_all.update(big_en); tri_all.update(tri_en)
        for k, s in big_en_entries.items(): big_all_entries[k] |= s
        for k, s in tri_en_entries.items(): tri_all_entries[k] |= s
    if has_de:
        big_all.update(big_de); tri_all.update(tri_de)
        for k, s in big_de_entries.items(): big_all_entries[k] |= s
        for k, s in tri_de_entries.items(): tri_all_entries[k] |= s

    df_top_bi_all  = rank_like_old_script(big_all_entries,  big_all,  TOP_K)
    df_top_tri_all = rank_like_old_script(tri_all_entries, tri_all, TOP_K)
    save_csv(df_top_bi_all,  "ngrams_top1000_bigrams_combined.csv")
    save_csv(df_top_tri_all, "ngrams_top1000_trigrams_combined.csv")

    print("Done.")

if __name__ == "__main__":
    main()
