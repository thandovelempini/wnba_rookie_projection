# WNBA Rookie Projection

**Can college statistics tell us which WNBA draftees will stick in the league-and how well they'll perform if they do?**

This project investigates two questions: which WNBA draftees return to the league in their second or third season, and which players go on to produce once they establish themselves. Using NCAA women's college box scores, I build statistical models in **R** and compare their predictions against draft position. The results are explored in an interactive **Tableau Public** dashboard.

[**Explore the interactive dashboard on Tableau Public**]()

![WNBA Rookie Projection dashboard preview]()

---

## Key findings

*The findings below reflect the current analysis and should be refreshed after the validation and metric checks described in the methodology.*

- **Draft position is a strong baseline for retention.** Draft slot alone achieved an AUC of 0.861, compared with 0.853 for the combined model and 0.800 for college statistics alone. In this analysis, college box-score statistics did not improve overall retention discrimination beyond draft position.
- **College statistics show promise for predicting production.** Among players who met the retention and playing-time criteria, the statistics-only model achieved an RMSE of 2.80 for Game Score per 40, compared with 3.06 for draft slot alone. The combined model achieved 2.92.
- **Retention and production are different questions.** A player's likelihood of returning to the league is not the same as her expected production once she has accumulated meaningful playing time. The project models these outcomes separately.
- **Draft position does not tell the whole story.** Jessica Shepard (2019, pick 16) was one of the model's notable positive disagreements with draft slot: the draft-slot model estimated a 49% retention probability, compared with 75% from the combined model. She subsequently returned to the league.
- **Future stars are a descriptive check, not the prediction target.** Of the 21 All-Stars identified in the training classes, 17 were top-seven picks. This provides context for how draft position relates to elite outcomes without treating a small group of stars as a separate modelling target.
- **A prospective prediction to investigate:** Madina Okot was assigned a 63% retention probability by the draft-slot model and 88% by the combined model. This is a model estimate, not a verified outcome.

These comparisons are intended to investigate whether college statistics add useful information beyond draft position-not to suggest that a model can replace scouting or explain every player's career.

## Research questions and outcomes

The project uses a two-stage approach.

| Stage | Outcome | Population |
|---|---|---|
| **1. Early-career retention** | Appeared in the WNBA in at least one of the second or third seasons after the draft year | Eligible US-college draftees in the training classes |
| **2. Early-career production** | Game Score per 40 minutes | Retained players with at least 300 WNBA minutes |

The draft year is treated as season one. Therefore, the retention outcome measures a return in season two or three, rather than simply whether a player ever appeared in the WNBA.

All-Star selections are included in the dashboard as retrospective context only. They are not a model target because the number of All-Stars is too small for a reliable standalone prediction model in this dataset.

- **Training classes:** 2018-2024
- **Prediction classes:** 2025-2026
- **Training sample:** 222 draftees
- **Observed retention rate:** 51%

The prediction classes are prospective cohorts in this analysis; their outcomes must be evaluated once the relevant seasons have been completed.

## Data

| Source | Data | Coverage |
|---|---|---|
| [wehoop](https://wehoop.sportsdataverse.org/) / ESPN | NCAA women's college box scores | 2016–17 to 2025–26 |
| [wehoop](https://wehoop.sportsdataverse.org/) / ESPN | WNBA player box scores | 2018–2026 |
| [Basketball Reference](https://www.basketball-reference.com/wnba/) | Draft results and player information | 2018–2026 |

College box scores are aggregated into player-season records. Draftees are matched to their college records using player names, school information and manual review where needed.

The college data collection begins with the 2016-17 season, subject to the coverage checks in the repository. The first modelled draft class is 2018.

## Methodology

### Feature engineering

College statistics are calculated from player box scores and include:

- **Production:** points, rebounds, assists, steals, blocks and turnovers per 40 minutes.
- **Shooting:** true shooting percentage, effective field-goal percentage, free-throw percentage, three-point attempt rate and free-throw rate.
- **Playing style and opportunity:** usage, minutes per game, games played and starting share.
- **Development:** final-season statistics and changes from the preceding season.
- **Competition:** team strength and opponent schedule strength, estimated using a simple rating system built from college game margins.
- **Draft context:** draft pick and position group.

Selected college statistics are standardised against the reference population in their respective seasons to make comparisons across seasons more interpretable.

### Models

Three model specifications are compared:

1. **Draft slot only:** establishes how much can be predicted from draft position alone.
2. **College statistics only:** evaluates the predictive signal in the college features.
3. **Combined model:** tests whether college statistics add information beyond draft position.

Regularised models are fitted using `glmnet`: logistic regression for retention and linear regression for production. In the combined model, draft position is left unpenalised while the additional predictors are regularised.

### Validation and evaluation

The intended evaluation design is **nested, grouped cross-validation by draft class**:

- The outer loop holds out draft classes for performance evaluation.
- The inner loop tunes model regularisation using only the remaining training classes.
- Competing models are evaluated on the same held-out classes.

Retention is evaluated using ROC AUC, with probability quality assessed using Brier score and log loss. Production is evaluated using root mean squared error (RMSE).

The final published results should be based on verified fold assignments and correctly calculated metrics. Cross-validation estimates measure performance on held-out historical classes; they do not guarantee future performance.

## Results

### Stage 1 — Early-career retention

*Current estimates; refresh after validating the grouped folds and classification metrics.*

| Model | ROC AUC | Brier score | Later-pick ROC AUC* |
|---|---:|---:|---:|
| Draft slot only | 0.861 | 0.160 | 0.732 |
| Combined model | 0.853 | 0.164 | 0.732 |
| College statistics only | 0.800 | To be verified | 0.603 |

\*The later-pick subset must be defined consistently using the actual draft rounds or clearly documented pick ranges before these figures are interpreted as a rounds two–three comparison.

The current results suggest that draft position captures a substantial amount of the signal associated with early-career retention. The important follow-up is whether college statistics are more useful for particular players or draft ranges, even when they do not improve the overall metric.

### Stage 2 — Production among retained players

*Current estimates; refresh after validating the grouped folds and evaluation metrics.*

| Model | RMSE (Game Score per 40) |
|---|---:|
| College statistics only | 2.80 |
| Combined model | 2.92 |
| Draft slot only | 3.06 |

Lower RMSE indicates smaller prediction errors on the evaluation sample. In the current estimates, college statistics alone perform best, suggesting that college performance may contain information about subsequent production that is not fully captured by draft position.

Game Score per 40 is a box-score-based production measure, not a complete measure of player quality or on-court impact.

## Tableau dashboard

The interactive dashboard contains five pages:

1. **Overview:** headline results and model comparisons.
2. **Draft vs model:** each draftee's estimated retention probability compared with the draft-slot baseline, highlighting disagreements.
3. **What drives it:** model coefficients and feature associations for retention and production.
4. **Player profiles:** draft details, model estimates, observed outcomes and college scoring trends.
5. **Did it find the stars?:** retrospective All-Star selections and each player's ranking within her draft class.

Model coefficients are associations within the fitted models, not causal effects. The dashboard is designed to explore where the models agree, where they disagree and what those disagreements might reveal about scouting.

## Repository structure

```text
wnba_rookie_projection/
├── src/
│   ├── 00_check_coverage.R
│   ├── 01_get_college.R
│   ├── 02_get_wnba.R
│   ├── 03_match_players.R
│   ├── 04_features.R
│   ├── model_utils.R
│   ├── 05_model.R
│   ├── 05b_extensions.R
│   ├── 06_export_tableau.R
│   └── 07_gauge_path.R
├── data/
│   ├── raw/
│   ├── clean/
│   └── tableau/
└── README.md
```

- `data/raw/` contains collected source data.
- `data/clean/` contains matched player records, engineered features and model outputs.
- `data/tableau/` contains the exported tables used by the dashboard.

## How to run

Run the scripts from the project root, the directory containing `src/`.

Install the required R packages:

```r
install.packages(c(
  "wehoop", "arrow", "tidyverse", "janitor", "rvest",
  "httr2", "stringdist", "tidymodels", "glmnet", "glue"
))
```

Then run the pipeline in order:

```r
source("src/00_check_coverage.R")
source("src/01_get_college.R")
source("src/02_get_wnba.R")
source("src/03_match_players.R")
source("src/04_features.R")
source("src/05_model.R")
source("src/05b_extensions.R")
source("src/06_export_tableau.R")
source("src/07_gauge_path.R")
```

Run the coverage checks first, and review the player-matching logs before interpreting the model results. Some scripts scrape Basketball Reference, so successful execution may depend on source availability and rate limits.

The project uses `arrow` alongside `wehoop` for data collection. Install the dependencies above before running the pipeline.

## Limitations

- **Sample size:** the training set contains 222 draftees across seven draft classes. Estimates may vary substantially across classes.
- **Opportunity versus ability:** returning to the league depends on more than performance, including injuries, roster availability and overseas commitments.
- **Box-score limitations:** the dataset does not include tracking data, physical measurements, detailed scouting reports or every factor used by professional teams.
- **International prospects:** players without US college records are excluded from the college-statistics modelling population.
- **Feature interpretation:** correlated predictors can produce unstable coefficients. Feature associations should not be interpreted as causal effects.
- **Conditional production target:** stage two includes only players who met the retention definition and accumulated at least 300 WNBA minutes. It does not predict production for every drafted player.
- **Prospective evaluation:** the 2025–2026 projections are unverified until the relevant outcome windows are complete.
- **Data matching and coverage:** missing box scores, ambiguous player identities and incomplete historical coverage can affect the results.

## Built with

**R · wehoop · tidyverse · tidymodels · glmnet · Tableau Public**

---

