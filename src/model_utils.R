# model_utils.R — shared nested cross-validation for 05_model.R and 05b_extensions.R
#
# Outer loop: hold out one draft class, used only for scoring.
# Inner loop: choose the penalty using only the remaining classes.
# The held-out class never influences tuning, so reported performance is unbiased.
#
# Outcome: `retained` = played in WNBA season 2 or 3 after the draft
# (draft-year season counts as season 1). "yes" is the event level.

library(tidyverse)
library(tidymodels)
library(glue)

tidymodels_prefer()

MODEL_TYPES <- c("draft_slot", "stats_only", "full")

# One fold per draft class (leave-one-class-out), set explicitly
class_folds <- function(data) {
  group_vfold_cv(data, group = draft_year, v = n_distinct(data$draft_year))
}

load_model_data <- function(path = "data/clean/model_data.csv") {
  read_csv(path, show_col_types = FALSE) |>
    mutate(retained = factor(retained, levels = c(TRUE, FALSE), labels = c("yes", "no")),
           log_pick    = log(pick))
}

stat_features <- function(md) {
  c(str_subset(names(md), "_z$"),
    "team_srs", "team_schedule_strength", "start_share", "mpg", "games",
    "pts_per40_change", "ts_pct_change", "usage_change", "mpg_change",
    "position_group")
}

make_recipe <- function(vars, outcome, data) {
  rec <- recipe(reformulate(vars, outcome), data = data) |>
    step_impute_median(all_numeric_predictors())
  if ("position_group" %in% vars) {
    rec <- rec |> step_unknown(position_group) |> step_dummy(position_group)
  }
  rec |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())
}

build_workflow <- function(type, outcome, mode, data, stats) {
  vars <- switch(type,
                 draft_slot = "log_pick",
                 stats_only = stats,
                 full       = c("log_pick", stats))
  rec <- make_recipe(vars, outcome, data)
  
  if (type == "draft_slot") {
    spec <- if (mode == "classification") logistic_reg() |> set_engine("glm")
    else linear_reg() |> set_engine("lm")
    return(workflow(rec, spec))
  }
  
  spec <- if (mode == "classification") logistic_reg(penalty = tune(), mixture = 0.5)
  else linear_reg(penalty = tune(), mixture = 0.5)
  
  if (type == "full") {
    # draft slot unpenalised: college stats must earn their place on top of it
    cols <- rec |> prep() |> bake(new_data = NULL) |> select(-all_of(outcome)) |> names()
    spec <- set_engine(spec, "glmnet", penalty.factor = as.numeric(cols != "log_pick"))
  } else {
    spec <- set_engine(spec, "glmnet")
  }
  workflow(rec, spec)
}

tune_and_fit <- function(type, outcome, mode, data, stats, tune_metric) {
  wf <- build_workflow(type, outcome, mode, data, stats)
  
  if (type != "draft_slot") {
    tune_set <- switch(tune_metric,
                       mn_log_loss = metric_set(mn_log_loss),
                       rmse        = metric_set(rmse))
    tuned <- tune_grid(wf, class_folds(data),
                       grid = grid_regular(penalty(), levels = 30), metrics = tune_set)
    wf <- finalize_workflow(wf, select_best(tuned, metric = tune_metric))
  }
  fit(wf, data)
}

predict_value <- function(fit, new_data, mode) {
  if (mode == "classification") predict(fit, new_data, type = "prob")$.pred_yes
  else predict(fit, new_data)$.pred
}

nested_cv <- function(data, outcome, mode, stats, tune_metric) {
  outer <- class_folds(data)
  
  map_dfr(outer$splits, function(split) {
    analysis_df <- analysis(split)
    assess_df   <- assessment(split)
    stopifnot(n_distinct(assess_df$draft_year) == 1)   # exactly one class held out
    message(glue("  held-out class {unique(assess_df$draft_year)}"))
    
    map_dfr(MODEL_TYPES, function(type) {
      fit <- tune_and_fit(type, outcome, mode, analysis_df, stats, tune_metric)
      tibble(model          = type,
             held_out_class = assess_df$draft_year,
             draft_year     = assess_df$draft_year,
             pick           = assess_df$pick,
             truth          = assess_df[[outcome]],
             .pred          = predict_value(fit, assess_df, mode))
    })
  })
}

fold_metrics <- function(preds, mode) {
  if (mode == "classification") {
    cls_eval <- metric_set(roc_auc, pr_auc, brier_class, mn_log_loss, accuracy)
    preds |>
      mutate(.pred_class = factor(if_else(.pred >= 0.5, "yes", "no"), levels = c("yes", "no"))) |>
      group_by(model, held_out_class) |>
      cls_eval(truth, .pred, estimate = .pred_class)
  } else {
    reg_eval <- metric_set(rmse, rsq, mae)
    preds |>
      group_by(model, held_out_class) |>
      reg_eval(truth, estimate = .pred)
  }
}

summarise_metrics <- function(folds) {
  folds |>
    group_by(model, .metric) |>
    summarise(mean = mean(.estimate), std_err = sd(.estimate) / sqrt(n()), .groups = "drop")
}

compare_to_slot <- function(folds, key_metric, higher_better) {
  folds |>
    filter(.metric == key_metric) |>
    select(model, held_out_class, .estimate) |>
    pivot_wider(names_from = model, values_from = .estimate) |>
    mutate(full_better = if (higher_better) full > draft_slot else full < draft_slot)
}

show_results <- function(folds, label, n, key_metric, higher_better) {
  message(glue("\n==== {label} (n = {n}) ===="))
  summarise_metrics(folds) |>
    mutate(mean = round(mean, 3)) |>
    select(-std_err) |>
    pivot_wider(names_from = .metric, values_from = mean) |>
    print()
  wins <- compare_to_slot(folds, key_metric, higher_better)
  message(glue("Full model beats draft slot on {key_metric} in ",
               "{sum(wins$full_better)} of {nrow(wins)} held-out classes"))
  invisible(wins)
}

calibration_table <- function(preds, bins = 5) {
  preds |>
    group_by(model) |>
    mutate(bin = ntile(.pred, bins)) |>
    group_by(model, bin) |>
    summarise(n              = n(),
              mean_predicted = mean(.pred),
              observed_rate  = mean(truth == "yes"),
              .groups        = "drop")
}