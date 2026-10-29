# 05b_extensions.R — (A) do stats help in rounds 2-3? (B) do stats predict impact among retained players?

source("src/model_utils.R")
set.seed(2026)

MIN_WNBA_MINUTES <- 300   # stage 2: enough minutes for a stable per-40 rate

md    <- load_model_data()
train <- filter(md, split == "train")
stats <- stat_features(md)

# ---- (A) Rounds 2 and 3 (actual draft rounds from Basketball Reference) -----
late <- filter(train, pick_round >= 2)
count(late, retained)

message("Rounds 2-3 - nested cross-validation:")
late_preds <- nested_cv(late, "retained", "classification", stats, tune_metric = "mn_log_loss")
late_folds <- fold_metrics(late_preds, "classification")
show_results(late_folds, "Rounds 2-3: early-career retention", nrow(late), "roc_auc",
             higher_better = TRUE)

# ---- (B) Stage 2: impact among retained players -----------------------------
# Conditional analysis: only players who were retained AND logged 300+ minutes.
# Game Score per 40 is a box-score contribution proxy, not a complete value measure.
stickers <- train |>
  filter(retained == "yes", wnba_minutes >= MIN_WNBA_MINUTES)

message(glue("Stage 2 target: Game Score per 40, mean {round(mean(stickers$wnba_gmsc_per40), 2)}, ",
             "sd {round(sd(stickers$wnba_gmsc_per40), 2)}"))

message("Stage 2 - nested cross-validation:")
impact_preds <- nested_cv(stickers, "wnba_gmsc_per40", "regression", stats, tune_metric = "rmse")
impact_folds <- fold_metrics(impact_preds, "regression")
show_results(impact_folds, "Retained players: Game Score per 40", nrow(stickers), "rmse",
             higher_better = FALSE)

impact_fit <- tune_and_fit("full", "wnba_gmsc_per40", "regression", stickers, stats, "rmse")

impact_coefs <- tidy(extract_fit_parsnip(impact_fit)) |>
  filter(term != "(Intercept)", estimate != 0) |>
  mutate(direction = if_else(estimate > 0, "raises impact", "lowers impact")) |>
  arrange(desc(abs(estimate)))

message("Stage 2 features kept (standardised):")
print(impact_coefs, n = Inf)

# ---- Save -------------------------------------------------------------------
bind_rows(
  mutate(summarise_metrics(late_folds),   analysis = "late_picks_retained"),
  mutate(summarise_metrics(impact_folds), analysis = "stickers_gmsc_per40")
) |>
  write_csv("data/clean/extension_results.csv")

write_csv(impact_coefs, "data/clean/impact_coefficients.csv")

# Three kinds of projection, kept apart and labelled:
#  - Validated:    stage 2 players, predicted when their draft class was held out
#  - Hypothetical: training-class players outside stage 2 (not retained or <300 min).
#                  The model never trained on them, but the number is "impact if she
#                  had been retained", which can't be checked against reality.
#  - Prospective:  2025-26 classes, from the final model; conditional on being retained
oof_impact <- impact_preds |>
  filter(model == "full") |>
  select(draft_year, pick, oof = .pred)

impact_projections <- md |>
  mutate(final_pred = predict_value(impact_fit, md, "regression")) |>
  left_join(oof_impact, by = c("draft_year", "pick")) |>
  mutate(
    projection_type = case_when(!is.na(oof)        ~ "Validated (out-of-fold)",
                                split == "predict" ~ "Prospective (if retained)",
                                TRUE               ~ "Hypothetical (if she had been retained)"),
    projected_gmsc_per40 = coalesce(oof, final_pred)
  ) |>
  select(draft_year, pick, player, projected_gmsc_per40, projection_type)

write_csv(impact_projections, "data/clean/impact_projections.csv")
count(impact_projections, projection_type)