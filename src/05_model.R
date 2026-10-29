# 05_model.R — does college production predict early-career retention beyond draft slot?

source("src/model_utils.R")
set.seed(2026)

md          <- load_model_data()
train       <- filter(md, split == "train")
predict_set <- filter(md, split == "predict")
stats       <- stat_features(md)

message("Nested cross-validation (this takes a few minutes):")
preds <- nested_cv(train, "retained", "classification", stats, tune_metric = "mn_log_loss")
folds <- fold_metrics(preds, "classification")

show_results(folds, "Early-career retention (played in season 2 or 3)", nrow(train), "roc_auc", higher_better = TRUE)

calibration <- calibration_table(preds)
message("Calibration (predicted vs observed rate, by quintile):")
print(mutate(calibration, across(c(mean_predicted, observed_rate), ~ round(.x, 2))), n = Inf)

write_csv(summarise_metrics(folds), "data/clean/model_results.csv")
write_csv(calibration, "data/clean/calibration.csv")

# ---- Final models: tuned on all training classes ----------------------------
final_fits <- set_names(MODEL_TYPES) |>
  map(~ tune_and_fit(.x, "retained", "classification", train, stats, "mn_log_loss"))

coefficients <- tidy(extract_fit_parsnip(final_fits$full)) |>
  filter(term != "(Intercept)", estimate != 0) |>
  mutate(direction = if_else(estimate < 0, "helps", "hurts")) |>   # glmnet models the "no" level
  arrange(desc(abs(estimate)))

write_csv(coefficients, "data/clean/model_coefficients.csv")
message("Features kept by the full model (standardised; 'helps' = raises chance of being retained):")
print(coefficients, n = Inf)

# ---- Predictions: out-of-fold for training classes, final model for 2025-26 --
train_preds <- preds |>
  select(model, draft_year, pick, .pred) |>
  pivot_wider(names_from = model, values_from = .pred, names_prefix = "p_")

new_preds <- predict_set |>
  select(draft_year, pick) |>
  mutate(p_draft_slot = predict_value(final_fits$draft_slot, predict_set, "classification"),
         p_stats_only = predict_value(final_fits$stats_only, predict_set, "classification"),
         p_full       = predict_value(final_fits$full,       predict_set, "classification"))

predictions <- md |>
  left_join(bind_rows(train_preds, new_preds), by = c("draft_year", "pick")) |>
  mutate(retained   = retained == "yes",
         value_vs_slot = p_full - p_draft_slot)

write_csv(predictions, "data/clean/predictions.csv")

message("Training classes: biggest model-vs-draft disagreements (out-of-fold)")
predictions |>
  filter(split == "train") |>
  arrange(desc(abs(value_vs_slot))) |>
  select(draft_year, pick, player, college, p_draft_slot, p_full, retained) |>
  mutate(across(starts_with("p_"), ~ round(.x, 2))) |>
  head(12) |>
  print()

message("2025-26 classes: model's highest-rated players")
predictions |>
  filter(split == "predict") |>
  arrange(desc(p_full)) |>
  select(draft_year, pick, player, college, p_draft_slot, p_full) |>
  mutate(across(starts_with("p_"), ~ round(.x, 2))) |>
  head(12) |>
  print()