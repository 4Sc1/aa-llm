
# BPD Event Text Processing & Modelling Scripts (R & Python)

This repository contains analysis and data-processing scripts for the manuscript *“Differential Reactivity of Affect and Self-Esteem in Borderline Personality Disorder to Daily Events: Contextual Insights from Large Language Models in Ambulatory Assessment”*.

All scripts are in a single folder. Inputs are CSV files (UTF-8); unless stated otherwise, columns are separated by semicolons `;`.

## Contents

- **`BPD_Bayes.R`** — Bayesian modelling script using `brms`; includes factor coding for event subcategories, event sentiment, and diary type.
- **`BPD_InterraterReliability.R`** — Inter-rater reliability script; the visible excerpt shows pairwise kappa calculation and output.
- **`BPD_LinearMixedEffects.R`** — Linear mixed-effects analyses, bootstrap procedures, \( R^2 \) summaries, covariance-structure comparisons, and plot generation.
- **`BPD_NaturalLanguageProcessingPipeline.py`** — Natural-language-processing pipeline that writes combined top-1000 bigram and trigram frequency tables.
- **`BPD_Ngrams_Event_Categorisation.py`** — Uses the OpenAI API to derive main event categories from frequent n-grams.
- **`BPD_PersonalDataCheckInTextEntries.py`** — Detects potential personal data in text entries using spaCy named-entity recognition and regex matching.
- **`BPD_Subevent_Categorisation.py`** — Contains the prompt used to derive parsimonious subcategories within broader main event categories.
- **`Lincense.txt`** — MIT licence file.

## Environment & Setup

### Python

- Python ≥ 3.10 recommended.
- Create a virtual environment and install packages:

```bash
python -m venv .venv
source .venv/bin/activate  # on Windows: .venv\Scripts\activate
pip install pandas openai spacy
python -m spacy download en_core_web_lg
python -m spacy download de_core_news_lg
```

- Set your OpenAI API key if you run one of the API-based scripts:

```bash
export OPENAI_API_KEY='your-key'   # PowerShell: $Env:OPENAI_API_KEY='your-key'
```

### R

- R ≥ 4.2 recommended.
- The R scripts include code for installing/loading packages at runtime.

## Scripts — Purpose, I/O, and Running

### `BPD_Bayes.R`

Bayesian modelling script using `brms`.

The visible excerpt shows:

- factor coding for `event_subcategory`
- factor coding for `event_sentiment`
- factor coding for `ediary_type`

The `event_subcategory` levels shown in the script are:

- Entertainment and Media Consumption
- Personal Care and Hygiene
- Commuting
- Meals and Snacks
- Family Dynamics
- Socializing and Leisure
- Travel and Excursions
- Cultural and Community Events
- Companion Animal Care
- Outdoor and Recreational Activities
- Rest and Sleep
- Medical Treatment and Healthcare Interactions
- Healthcare Journeys and Visits
- Shopping and Errands
- Household Chores
- Emotional and Conflict Resolution
- Work-Related Tasks and Projects
- Mental Health and Emotional State
- Indeterminate
- Romantic Partnerships
- Mood and Emotional Fluctuations
- Friendship Activities
- Acquaintance and Casual Interactions
- Educational and Academic Work
- Professional Communication and Meetings
- Relaxation
- Leisure and Entertainment at Home
- Reading and Intellectual Activities
- Games and Hobbies
- Financial and Administrative Tasks
- Physical Health and Symptoms
- Sports and Physical Activities

Reference levels shown in the excerpt:

- `event_subcategory`: `"Meals and Snacks"`
- `event_sentiment`: `"neutral"`
- `ediary_type`: `"2"`

The visible excerpt also shows saving model summaries to CSV:

```r
for (nm in names(models)) {
  save_brms_summary_as_csv(models[[nm]], nm, output_base_path)
}
```

### `BPD_InterraterReliability.R`

Inter-rater reliability script.

The visible excerpt shows pairwise kappa calculation:

```r
pairwise_kappa_results <- calculate_pairwise_kappa(fleiss_data[, -1])  
print(pairwise_kappa_results)
```

Because only a short excerpt was attached, no additional claims are made here beyond the visible code.

### `BPD_LinearMixedEffects.R`

Linear mixed-effects analysis script for the manuscript.

The visible excerpts show that the script includes:

- package installation/loading helper code
- logging to file
- construction of an `analysis_df`
- derivation of `day` and `prompt_in_day`
- p-value formatting helpers
- bootstrap functions
- covariance-structure comparison and sensitivity refits
- plot theme and export helpers
- model fitting and plotting for:
  - `self_esteem`
  - `valence`
  - `calmness`

Variables explicitly visible in the excerpts include:

- `prompt`
- `day`
- `event_evaluation_centered`
- `event_sentiment`
- `event_sentiment_encoded`
- `sentiment_within`
- `sentiment_mean`
- `self_esteem`
- `valence`
- `calmness`
- `id`
- `group`
- `event_category`
- `ediary_type`
- `event_subcategory`
- `BSL_score`
- `BPD_severity`

The excerpt also shows the following derived variables:

```r
df$BPD_severity <- df$bpdanz
df$BSL_score <- as.numeric(df$BSL_T1_mean)
```

and a check that `BSL_T1_mean` exists in the data.

Example plotting calls shown in the excerpt include:

- `plot_m1_event_by_group(...)`
- `plot_m2_sentiment_by_group(...)`

### `BPD_NaturalLanguageProcessingPipeline.py`

Natural-language-processing pipeline.

The visible excerpt shows that the script writes:

- `ngrams_top1000_bigrams_combined.csv`
- `ngrams_top1000_trigrams_combined.csv`

using:

```python
save_csv(df_top_bi_all,  "ngrams_top1000_bigrams_combined.csv")
save_csv(df_top_tri_all, "ngrams_top1000_trigrams_combined.csv")
```

and is run through:

```python
if __name__ == "__main__":
    main()
```

### `BPD_Ngrams_Event_Categorisation.py`

Script for deriving main event categories from frequent n-grams using the OpenAI API.

The attached file states:

**Input:**
- `ngrams_top1000_bigrams_de.csv`
- `ngrams_top1000_trigrams_de.csv`

**Output:**
- `ngram_derived_main_categories_raw.txt`
- `ngram_derived_main_categories.txt`

The visible excerpt also shows:

- use of the OpenAI client
- `model = "gpt-3.5-turbo"`
- `temperature = 0.2`
- `max_tokens = 1200`
- `timeout_seconds = 60`

and the logic:

```python
if not any(c.strip().lower() == "indeterminate" for c in cats):
    cats.append("Indeterminate")
```

The script contains placeholder paths that must be edited before execution.

### `BPD_PersonalDataCheckInTextEntries.py`

Script for detecting potentially identifying information in text entries.

The visible excerpt shows that the following columns are scanned:

- `event_original`
- `event_corrected_de`
- `event_en`

The file loads the spaCy models:

- `de_core_news_lg`
- `en_core_web_lg`

Named-entity labels of interest are:

- `PERSON`
- `GPE`
- `LOC`
- `ORG`
- `NORP`
- `DATE`
- `TIME`

The regexes shown in the excerpt detect:

- email addresses
- phone numbers
- URLs
- handles
- IBANs

### `BPD_Subevent_Categorisation.py`

Script containing the prompt for deriving subcategories within broader main event categories.

The visible excerpt shows a `system_prompt` instructing the model to act as a clinical psychologist and to propose a parsimonious set of meaningful subcategories within a given main category.

The prompt explicitly defines these main categories:

1. **Daily Routines and Household Activities**
2. **Leisure and Recreation**
3. **Social and Personal Relationships**
4. **Work and Professional Engagements**

The visible excerpt ends partway through the fourth category definition, so no further claims are made here about the remainder of the file.

### `Lincense.txt`

MIT License file.

The attached file name is `Lincense.txt`.

## Notes

- Several files contain placeholder paths such as `<path to ...>` and require manual editing before use.
- Some files are only partially visible in the attached excerpts; this README therefore describes only what is directly supported by those excerpts.
- Scripts using the OpenAI API require a valid API key.

## Detected Dependencies

### R packages

MuMIn, boot, car, distributional, dplyr, forecast, ggeffects, ggplot2, ggtext, gridExtra, htmltools, irr, irrCAC, knitr, lme4, lmerTest, magrittr, multilevelTools, naniar, pROC, papaja, patchwork, plotly, psych, raters, readr, report, sjPlot, tibble, tidybayes, utils, webshot

### Python packages

contractions, language-tool-python, openai, pandas, spacy, tqdm

## Software citations (R)

* Bartoń, K. (2025). *MuMIn: Multi-Model Inference* (v1.48.11) [R package]. CRAN. [https://CRAN.R-project.org/package=MuMIn](https://CRAN.R-project.org/package=MuMIn). [https://doi.org/10.32614/CRAN.package.MuMIn](https://doi.org/10.32614/CRAN.package.MuMIn)

* Canty, A., & Ripley, B. D. (2024). *boot: Bootstrap R (S-Plus) Functions* (v1.3-31) [R package].

* Davison, A. C., & Hinkley, D. V. (1997). *Bootstrap Methods and Their Applications*. Cambridge University Press. [https://doi.org/10.1017/CBO9780511802843](https://doi.org/10.1017/CBO9780511802843)

* Bürkner, P.-C. (2017). *brms: An R package for Bayesian multilevel models using Stan*. *Journal of Statistical Software, 80*(1), 1–28. [https://doi.org/10.18637/jss.v080.i01](https://doi.org/10.18637/jss.v080.i01). ([Journal of Statistical Software][1])

* Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression* (3rd ed.). Sage. [https://www.john-fox.ca/Companion/](https://www.john-fox.ca/Companion/)

* O’Hara-Wild, M., Kay, M., Hayes, A., & Hyndman, R. (2024). *distributional: Vectorised Probability Distributions* (v0.5.0) [R package]. CRAN. [https://CRAN.R-project.org/package=distributional](https://CRAN.R-project.org/package=distributional). [https://doi.org/10.32614/CRAN.package.distributional](https://doi.org/10.32614/CRAN.package.distributional)

* Wickham, H., François, R., Henry, L., Müller, K., & Vaughan, D. (2023). *dplyr: A Grammar of Data Manipulation* (v1.1.4) [R package]. CRAN. [https://CRAN.R-project.org/package=dplyr](https://CRAN.R-project.org/package=dplyr). [https://doi.org/10.32614/CRAN.package.dplyr](https://doi.org/10.32614/CRAN.package.dplyr)

* Hyndman, R., Athanasopoulos, G., Bergmeir, C., Caceres, G., Chhay, L., O’Hara-Wild, M., Petropoulos, F., Razbash, S., Wang, E., & Yasmeen, F. (2025). *forecast: Forecasting functions for time series and linear models* (v8.24.0) [R package]. [https://pkg.robjhyndman.com/forecast/](https://pkg.robjhyndman.com/forecast/)

* Hyndman, R. J., & Khandakar, Y. (2008). Automatic time series forecasting: the forecast package for R. *Journal of Statistical Software, 27*(3), 1–22. [https://doi.org/10.18637/jss.v027.i03](https://doi.org/10.18637/jss.v027.i03)

* Lüdecke, D. (2018). ggeffects: Tidy data frames of marginal effects from regression models. *Journal of Open Source Software, 3*(26), 772. [https://doi.org/10.21105/joss.00772](https://doi.org/10.21105/joss.00772)

* Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag. [https://ggplot2.tidyverse.org](https://ggplot2.tidyverse.org)

* Wilke, C. O., & Wiernik, B. M. (2022). *ggtext: Improved Text Rendering Support for ‘ggplot2’* (v0.1.2) [R package]. CRAN. [https://CRAN.R-project.org/package=ggtext](https://CRAN.R-project.org/package=ggtext). [https://doi.org/10.32614/CRAN.package.ggtext](https://doi.org/10.32614/CRAN.package.ggtext)

* Auguie, B. (2017). *gridExtra: Miscellaneous Functions for “Grid” Graphics* (v2.3) [R package]. CRAN. [https://CRAN.R-project.org/package=gridExtra](https://CRAN.R-project.org/package=gridExtra). [https://doi.org/10.32614/CRAN.package.gridExtra](https://doi.org/10.32614/CRAN.package.gridExtra)

* Cheng, J., Sievert, C., Schloerke, B., Chang, W., Xie, Y., & Allen, J. (2024). *htmltools: Tools for HTML* (v0.5.8.1) [R package]. CRAN. [https://CRAN.R-project.org/package=htmltools](https://CRAN.R-project.org/package=htmltools). [https://doi.org/10.32614/CRAN.package.htmltools](https://doi.org/10.32614/CRAN.package.htmltools)

* Gamer, M., Lemon, J., Fellows, I., & Singh, P. (2019). *irr: Various Coefficients of Interrater Reliability and Agreement* (v0.84.1) [R package]. CRAN. [https://CRAN.R-project.org/package=irr](https://CRAN.R-project.org/package=irr). [https://doi.org/10.32614/CRAN.package.irr](https://doi.org/10.32614/CRAN.package.irr)

* Gwet, K. L. (2019). *irrCAC: Computing Chance-Corrected Agreement Coefficients (CAC)* (v1.0) [R package]. CRAN. [https://CRAN.R-project.org/package=irrCAC](https://CRAN.R-project.org/package=irrCAC). [https://doi.org/10.32614/CRAN.package.irrCAC](https://doi.org/10.32614/CRAN.package.irrCAC)

* Xie, Y. (2025). *knitr: A General-Purpose Package for Dynamic Report Generation in R* (v1.50) [R package]. [https://yihui.org/knitr/](https://yihui.org/knitr/)

* Xie, Y. (2015). *Dynamic Documents with R and knitr* (2nd ed.). Chapman & Hall/CRC. [https://yihui.org/knitr/](https://yihui.org/knitr/)

* Xie, Y. (2014). knitr: A comprehensive tool for reproducible research in R. In V. Stodden, F. Leisch, & R. D. Peng (Eds.), *Implementing Reproducible Computational Research*. Chapman & Hall/CRC.

* Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. *Journal of Statistical Software, 67*(1), 1–48. [https://doi.org/10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01)

* Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). lmerTest package: Tests in linear mixed effects models. *Journal of Statistical Software, 82*(13), 1–26. [https://doi.org/10.18637/jss.v082.i13](https://doi.org/10.18637/jss.v082.i13)

* Bache, S. M., & Wickham, H. (2025). *magrittr: A Forward-Pipe Operator for R* (v2.0.4) [R package]. CRAN. [https://CRAN.R-project.org/package=magrittr](https://CRAN.R-project.org/package=magrittr). [https://doi.org/10.32614/CRAN.package.magrittr](https://doi.org/10.32614/CRAN.package.magrittr)

* Wiley, J. F. (2025). *multilevelTools: Multilevel and Mixed Effects Model Diagnostics and Effect Sizes* (v0.2.1) [R package]. CRAN. [https://CRAN.R-project.org/package=multilevelTools](https://CRAN.R-project.org/package=multilevelTools). [https://doi.org/10.32614/CRAN.package.multilevelTools](https://doi.org/10.32614/CRAN.package.multilevelTools)

* Tierney, N., & Cook, D. (2023). Expanding tidy data principles to facilitate missing data exploration, visualization and assessment of imputations. *Journal of Statistical Software, 105*(7), 1–31. [https://doi.org/10.18637/jss.v105.i07](https://doi.org/10.18637/jss.v105.i07)

* Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J.-C., & Müller, M. (2011). pROC: an open-source package for R and S+ to analyse and compare ROC curves. *BMC Bioinformatics, 12*, 77.

* Aust, F., & Barth, M. (2024). *papaja: Prepare reproducible APA journal articles with R Markdown* (v0.1.3) [R package]. GitHub. [https://github.com/crsh/papaja](https://github.com/crsh/papaja). [https://doi.org/10.32614/CRAN.package.papaja](https://doi.org/10.32614/CRAN.package.papaja)

* Pedersen, T. L. (2025). *patchwork: The Composer of Plots* (v1.3.2) [R package]. CRAN. [https://CRAN.R-project.org/package=patchwork](https://CRAN.R-project.org/package=patchwork). [https://doi.org/10.32614/CRAN.package.patchwork](https://doi.org/10.32614/CRAN.package.patchwork)

* Sievert, C. (2020). *Interactive Web-Based Data Visualization with R, plotly, and shiny*. Chapman & Hall/CRC. [https://plotly-r.com](https://plotly-r.com)

* Revelle, W. (2025). *psych: Procedures for Psychological, Psychometric, and Personality Research* (v2.5.6) [R package]. CRAN. [https://CRAN.R-project.org/package=psych](https://CRAN.R-project.org/package=psych)

* Quatto, P., & Ripamonti, E. (2024). *raters: A Modification of Fleiss’ Kappa in Case of Nominal and Ordinal Variables* (v2.1.1) [R package]. CRAN. [https://CRAN.R-project.org/package=raters](https://CRAN.R-project.org/package=raters). [https://doi.org/10.32614/CRAN.package.raters](https://doi.org/10.32614/CRAN.package.raters)

* Wickham, H., Hester, J., & Bryan, J. (2024). *readr: Read Rectangular Text Data* (v2.1.5) [R package]. CRAN. [https://CRAN.R-project.org/package=readr](https://CRAN.R-project.org/package=readr). [https://doi.org/10.32614/CRAN.package.readr](https://doi.org/10.32614/CRAN.package.readr)

* Makowski, D., Lüdecke, D., Patil, I., Thériault, R., Ben-Shachar, M. S., & Wiernik, B. M. (2023). Automated results reporting as a practical tool to improve reproducibility and methodological best practices adoption. *report* project website. [https://easystats.github.io/report/](https://easystats.github.io/report/)

* Lüdecke, D. (2025). *sjPlot: Data Visualization for Statistics in Social Science* (v2.9.0) [R package]. CRAN. [https://CRAN.R-project.org/package=sjPlot](https://CRAN.R-project.org/package=sjPlot)

* Müller, K., & Wickham, H. (2025). *tibble: Simple Data Frames* (v3.3.0) [R package]. CRAN. [https://CRAN.R-project.org/package=tibble](https://CRAN.R-project.org/package=tibble). [https://doi.org/10.32614/CRAN.package.tibble](https://doi.org/10.32614/CRAN.package.tibble)

* Kay, M. (2024). *tidybayes: Tidy Data and Geoms for Bayesian Models* (v3.0.7) [R package]. [https://mjskay.github.io/tidybayes/](https://mjskay.github.io/tidybayes/). [https://doi.org/10.5281/zenodo.1308151](https://doi.org/10.5281/zenodo.1308151)

* R Core Team. (2025). *R: A Language and Environment for Statistical Computing*. R Foundation for Statistical Computing. [https://www.R-project.org/](https://www.R-project.org/)

* Chang, W. (2023). *webshot: Take Screenshots of Web Pages* (v0.5.5) [R package]. CRAN. [https://CRAN.R-project.org/package=webshot](https://CRAN.R-project.org/package=webshot). [https://doi.org/10.32614/CRAN.package.webshot](https://doi.org/10.32614/CRAN.package.webshot)

# Software Cytations (Python)

* *Jinja2* (v3.1.6) [Python package]. [https://github.com/pallets/jinja/](https://github.com/pallets/jinja/)

* *MarkupSafe* (v3.0.3) [Python package]. [https://github.com/pallets/markupsafe/](https://github.com/pallets/markupsafe/)
* Brandl, G. *Pygments* (v2.19.2) [Python package]. [https://pygments.org](https://pygments.org)

* Garcia Badaracco, A., Colvin, S., & Hatfield-Dodds, Z. *annotated-types* (v0.7.0) [Python package]. [https://github.com/annotated-types/annotated-types](https://github.com/annotated-types/annotated-types)

* Hunter WB. *anyascii* (v0.3.3) [Python package]. [https://github.com/anyascii/anyascii](https://github.com/anyascii/anyascii)

* Grönholm, A. *anyio* (v4.11.0) [Python package]. [https://anyio.readthedocs.io/en/stable/versionhistory.html](https://anyio.readthedocs.io/en/stable/versionhistory.html)

* Honnibal, M. *blis* (v1.3.0) [Python package]. [https://github.com/explosion/cython-blis](https://github.com/explosion/cython-blis)

* Explosion. *catalogue* (v2.0.10) [Python package]. [https://github.com/explosion/catalogue](https://github.com/explosion/catalogue)

* Reitz, K. *certifi* (v2025.10.5) [Python package]. [https://github.com/certifi/python-certifi](https://github.com/certifi/python-certifi)

* “Ahmed R. TAHRI”. *charset-normalizer* (v3.4.4) [Python package]. [https://github.com/jawah/charset_normalizer/blob/master/CHANGELOG.md](https://github.com/jawah/charset_normalizer/blob/master/CHANGELOG.md)

* *click* (v8.3.0) [Python package]. [https://github.com/pallets/click/](https://github.com/pallets/click/)

* DrivenData. *cloudpathlib* (v0.23.0) [Python package]. [https://github.com/drivendataorg/cloudpathlib](https://github.com/drivendataorg/cloudpathlib)

* Explosion. *confection* (v0.1.5) [Python package]. [https://github.com/explosion/confection](https://github.com/explosion/confection)

* van Kooten, P. *contractions* (v0.1.73) [Python package]. [https://github.com/kootenpv/contractions](https://github.com/kootenpv/contractions)

* Honnibal, M. *cymem* (v2.0.11) [Python package]. [https://github.com/explosion/cymem](https://github.com/explosion/cymem)

* Cohen, N. *distro* (v1.9.0) [Python package]. [https://github.com/python-distro/distro](https://github.com/python-distro/distro)

* Smith, N. J. *h11* (v0.16.0) [Python package]. [https://github.com/python-hyper/h11](https://github.com/python-hyper/h11)

* Christie, T. *httpcore* (v1.0.9) [Python package]. [https://www.encode.io/httpcore/](https://www.encode.io/httpcore/)

* Christie, T. *httpx* (v0.28.1) [Python package]. [https://github.com/encode/httpx](https://github.com/encode/httpx)

* Davies, K. *idna* (v3.11) [Python package]. [https://github.com/kjd/idna](https://github.com/kjd/idna)

* Colvin, S. *jiter* (v0.11.1) [Python package]. [https://github.com/pydantic/jiter/](https://github.com/pydantic/jiter/)

* Speer, E. R. *langcodes* (v3.5.0) [Python package]. [https://github.com/georgkrause/langcodes](https://github.com/georgkrause/langcodes)

* Speer, E. R. *language_data* (v1.3.0) [Python package]. [https://github.com/georgkrause/language_data](https://github.com/georgkrause/language_data)

* Korobov, M. *marisa-trie* (v1.3.1) [Python package]. [https://github.com/pytries/marisa-trie](https://github.com/pytries/marisa-trie)

* Sewell, C. *markdown-it-py* (v4.0.0) [Python package]. [https://github.com/executablebooks/markdown-it-py](https://github.com/executablebooks/markdown-it-py)

* Hukkinen, T. *mdurl* (v0.1.2) [Python package]. [https://github.com/executablebooks/mdurl](https://github.com/executablebooks/mdurl)

* Explosion. *murmurhash* (v1.0.13) [Python package]. [https://github.com/explosion/murmurhash](https://github.com/explosion/murmurhash)

* Oliphant, T. E., *et al.* *NumPy* (v2.3.4) [Python package]. [https://numpy.org](https://numpy.org)

* OpenAI. *openai* (v2.6.0) [Python package]. [https://github.com/openai/openai-python](https://github.com/openai/openai-python)

* Stufft, D. *packaging* (v25.0) [Python package]. [https://github.com/pypa/packaging](https://github.com/pypa/packaging)

* The Pandas Development Team. *pandas* (v2.3.3) [Python package]. [https://pandas.pydata.org](https://pandas.pydata.org)

* Explosion. *preshed* (v3.0.10) [Python package]. [https://github.com/explosion/preshed](https://github.com/explosion/preshed)

* Muła, W. *pyahocorasick* (v2.2.0) [Python package]. [http://github.com/WojciechMula/pyahocorasick](http://github.com/WojciechMula/pyahocorasick)

* Colvin, S., Jolibois, E., Ramezani, H., Garcia Badaracco, A., Dorsey, T., Montague, D., Matveenko, S., Trylesinski, M., Runkle, S., Hewitt, D., Hall, A., Plot, V., & Maan, D. *pydantic* (v2.12.3) [Python package]. [https://github.com/pydantic/pydantic](https://github.com/pydantic/pydantic)

* Colvin, S., Garcia Badaracco, A., Montague, D., Hewitt, D., Runkle, S., & Plot, V. *pydantic-core* (v2.41.4) [Python package]. [https://github.com/pydantic/pydantic-core](https://github.com/pydantic/pydantic-core)

* Niemeyer, G. *python-dateutil* (v2.9.0.post0) [Python package]. [https://github.com/dateutil/dateutil](https://github.com/dateutil/dateutil)

* Bishop, S. *pytz* (v2025.2) [Python package]. [http://pythonhosted.org/pytz](http://pythonhosted.org/pytz)

* Reitz, K. *requests* (v2.32.5) [Python package]. [https://requests.readthedocs.io](https://requests.readthedocs.io)

* McGugan, W. *rich* (v14.2.0) [Python package]. [https://github.com/Textualize/rich](https://github.com/Textualize/rich)

* Chung, T.-p. *shellingham* (v1.5.4) [Python package]. [https://github.com/sarugaku/shellingham](https://github.com/sarugaku/shellingham)

* Peterson, B. *six* (v1.17.0) [Python package]. [https://github.com/benjaminp/six](https://github.com/benjaminp/six)

* Řehůřek, R. *smart_open* (v7.4.1) [Python package]. [https://github.com/piskvorky/smart_open](https://github.com/piskvorky/smart_open)

* Smith, N. J. *sniffio* (v1.3.1) [Python package]. [https://github.com/python-trio/sniffio](https://github.com/python-trio/sniffio)

* Explosion. *spaCy* (v3.8.7) [Python package]. [https://spacy.io](https://spacy.io)

* Explosion. *spacy-legacy* (v3.0.12) [Python package]. [https://spacy.io](https://spacy.io)

* Explosion. *spacy-loggers* (v1.0.5) [Python package]. [https://github.com/explosion/spacy-loggers](https://github.com/explosion/spacy-loggers)

* Explosion. *srsly* (v2.5.1) [Python package]. [https://github.com/explosion/srsly](https://github.com/explosion/srsly)

* van Kooten, P. *textsearch* (v0.0.24) [Python package]. [https://github.com/kootenpv/textsearch](https://github.com/kootenpv/textsearch)

* Explosion. *thinc* (v8.3.6) [Python package]. [https://github.com/explosion/thinc](https://github.com/explosion/thinc)

* *tqdm* (v4.67.1) [Python package]. [https://tqdm.github.io](https://tqdm.github.io)

* Ramírez, S. *typer* (v0.20.0) [Python package]. [https://github.com/fastapi/typer](https://github.com/fastapi/typer)

* Plot, V. *typing-inspection* (v0.4.2) [Python package]. [https://github.com/pydantic/typing-inspection](https://github.com/pydantic/typing-inspection)

* “Guido van Rossum, Jukka Lehtosalo, Łukasz Langa, Michael Lee”. *typing_extensions* (v4.15.0) [Python package]. [https://github.com/python/typing_extensions](https://github.com/python/typing_extensions)

* Python Software Foundation. *tzdata* (v2025.2) [Python package]. [https://github.com/python/tzdata](https://github.com/python/tzdata)

* Petrov, A. *urllib3* (v2.5.0) [Python package]. [https://github.com/urllib3/urllib3/blob/main/CHANGES.rst](https://github.com/urllib3/urllib3/blob/main/CHANGES.rst)

* Explosion. *wasabi* (v1.1.3) [Python package]. [https://github.com/explosion/wasabi](https://github.com/explosion/wasabi)

* Explosion. *weasel* (v0.4.1) [Python package]. [https://github.com/explosion/weasel/](https://github.com/explosion/weasel/)

* Dumpleton, G. *wrapt* (v2.0.0) [Python package]. [https://github.com/GrahamDumpleton/wrapt](https://github.com/GrahamDumpleton/wrapt)