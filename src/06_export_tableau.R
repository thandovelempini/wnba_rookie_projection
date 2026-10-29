# 06_export_tableau.R — All-Star labels + Tableau-ready tables

library(tidyverse)
library(rvest)
library(glue)

ALL_STAR_YEARS <- c(2018, 2019, 2021:2026)   # no All-Star Game in 2020
LAST_SEASON    <- 2026
OUT_DIR        <- "data/tableau"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

normalise_name <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z ]", "") |>
    str_squish()
}

# ---- All-Star Game participants ---------------------------------------------
get_all_stars <- function(year) {
  message(glue("Fetching {year} All-Star Game..."))
  Sys.sleep(4)
  page <- tryCatch(
    read_html(glue("https://www.basketball-reference.com/wnba/allstar/WNBA_{year}.html")),
    error = function(e) {
      message(glue("  -> failed: {conditionMessage(e)}"))
      NULL
    }
  )
  if (is.null(page)) return(tibble())
  
  tibble(all_star_year = year,
         player        = unique(html_text2(html_elements(page, "table a[href*='/wnba/players/']"))))
}

all_stars <- map_dfr(ALL_STAR_YEARS, get_all_stars) |>
  mutate(name_key = normalise_name(player))

write_csv(all_stars, "data/raw/wnba_all_stars.csv")

# ---- Inputs -----------------------------------------------------------------
predictions <- read_csv("data/clean/predictions.csv", show_col_types = FALSE)
impact      <- read_csv("data/clean/impact_projections.csv", show_col_types = FALSE)
dc          <- read_csv("data/clean/draftee_college_seasons.csv", show_col_types = FALSE)

star_summary <- predictions |>
  select(draft_year, pick, player) |>
  mutate(name_key = normalise_name(player)) |>
  inner_join(select(all_stars, all_star_year, name_key), by = "name_key",
             relationship = "many-to-many") |>
  filter(all_star_year >= draft_year) |>
  group_by(draft_year, pick) |>
  summarise(all_star_selections = n_distinct(all_star_year),
            first_all_star      = min(all_star_year),
            all_star_within_5   = any(all_star_year <= draft_year + 4),
            .groups = "drop")

# ---- 1. Players: one row per draftee ----------------------------------------
players <- predictions |>
  left_join(select(impact, draft_year, pick, projected_gmsc_per40, projection_type), by = c("draft_year", "pick")) |>
  left_join(star_summary, by = c("draft_year", "pick")) |>
  mutate(
    all_star_selections = replace_na(all_star_selections, 0),
    all_star_within_5   = replace_na(all_star_within_5, FALSE),
    expected_value      = p_full * projected_gmsc_per40,
    model_view = case_when(value_vs_slot >=  0.15 ~ "Model higher than draft slot",
                           value_vs_slot <= -0.15 ~ "Model lower than draft slot",
                           TRUE                   ~ "Agrees with draft slot"),
    outcome    = case_when(split == "predict" ~ "Too early to tell",
                           retained           ~ "Retained",
                           TRUE               ~ "Not retained")
  ) |>
  group_by(draft_year) |>
  mutate(model_rank = min_rank(desc(p_full))) |>
  ungroup() |>
  transmute(
    `Draft Year`                  = draft_year,
    Pick                          = pick,
    Round                         = pick_round,
    Player                        = player,
    College                       = college,
    `Final College Team`          = final_team,
    Position                      = position_group,
    `Draft Team`                  = draft_team,
    Split                         = if_else(split == "train", "Training class", "Prediction class"),
    Outcome                       = outcome,
    `Retained (Season 2 or 3)`    = retained,
    `WNBA Seasons (First 3)`      = seasons_played,
    `WNBA Games (First 3)`        = wnba_games,
    `WNBA Minutes (First 3)`      = wnba_minutes,
    `WNBA Game Score per 40`      = wnba_gmsc_per40,
    `First WNBA Season`           = first_wnba_season,
    `All-Star`                    = all_star_selections > 0,
    `All-Star Selections`         = all_star_selections,
    `First All-Star`              = first_all_star,
    `All-Star Within 5 Seasons`   = all_star_within_5,
    `5-Season Window Complete`    = draft_year + 4 <= LAST_SEASON,
    `P(Retained) - Draft Slot`       = p_draft_slot,
    `P(Retained) - Stats Only`       = p_stats_only,
    `P(Retained) - Full Model`       = p_full,
    `Model vs Draft Slot`         = value_vs_slot,
    `Model View`                  = model_view,
    `Model Rank in Class`         = model_rank,
    `Projected Game Score per 40` = projected_gmsc_per40,
    `Projection Type`             = projection_type,
    `Expected Value`              = expected_value,
    `College Games`               = games,
    `College MPG`                 = mpg,
    `College Pts per 40`          = pts_per40,
    `College Reb per 40`          = trb_per40,
    `College Ast per 40`          = ast_per40,
    `College Stl per 40`          = stl_per40,
    `College Blk per 40`          = blk_per40,
    `College TS%`                 = ts_pct,
    `College eFG%`                = efg_pct,
    `College 3PA Rate`            = fg3a_rate,
    `College FT%`                 = ft_pct,
    `College Usage`               = usage,
    `Team Rating`                 = team_srs,
    `Schedule Strength`           = team_schedule_strength,
    `Scoring (z)`                 = pts_per40_z,
    `Efficiency (z)`              = ts_pct_z,
    `Usage (z)`                   = usage_z,
    `Rebounding (z)`              = trb_per40_z
  )

write_csv(players, file.path(OUT_DIR, "players.csv"), na = "")

# ---- 2. Model performance ---------------------------------------------------
pretty_model <- c(draft_slot = "Draft slot only",
                  stats_only = "College stats only",
                  full       = "Draft slot + college stats")

pretty_metric <- c(roc_auc = "AUC", pr_auc = "PR-AUC", brier_class = "Brier score",
                   mn_log_loss = "Log loss", accuracy = "Accuracy",
                   rmse = "RMSE", rsq = "R-squared", mae = "MAE")

pretty_analysis <- c(retained            = "Early-career retention - all picks",
                     late_picks_retained = "Early-career retention - rounds 2-3",
                     stickers_gmsc_per40    = "Impact among retained players")

performance <- bind_rows(
  read_csv("data/clean/model_results.csv", show_col_types = FALSE) |> mutate(analysis = "retained"),
  read_csv("data/clean/extension_results.csv", show_col_types = FALSE)
) |>
  transmute(
    Analysis           = unname(pretty_analysis[analysis]),
    Model              = unname(pretty_model[model]),
    Metric             = unname(pretty_metric[.metric]),
    Value              = mean,
    `Std Error`        = std_err,
    `Higher Is Better` = !.metric %in% c("brier_class", "mn_log_loss", "rmse", "mae")
  )

write_csv(performance, file.path(OUT_DIR, "model_performance.csv"), na = "")

# ---- 3. Feature effects -----------------------------------------------------
feature_labels <- c(
  log_pick = "Draft pick (log)", team_srs = "Team rating",
  team_schedule_strength = "Schedule strength", start_share = "Start share",
  mpg = "Minutes per game", games = "Games played",
  pts_per40_change = "Scoring change (final year)", ts_pct_change = "TS% change (final year)",
  usage_change = "Usage change (final year)", mpg_change = "Minutes change (final year)",
  pts_per40_z = "Scoring", trb_per40_z = "Rebounding", orb_per40_z = "Offensive rebounding",
  drb_per40_z = "Defensive rebounding", ast_per40_z = "Assists", stl_per40_z = "Steals",
  blk_per40_z = "Blocks", tov_per40_z = "Turnovers", ts_pct_z = "True shooting %",
  efg_pct_z = "Effective FG%", fg3a_rate_z = "3-point attempt rate",
  ft_rate_z = "Free throw rate", ft_pct_z = "Free throw %",
  ast_tov_z = "Assist/turnover ratio", usage_z = "Usage rate"
)

pretty_feature <- function(term) {
  out <- unname(feature_labels[term])
  out[is.na(out)] <- str_replace_all(term[is.na(out)], "_", " ")
  out
}

effects <- bind_rows(
  # stage 1 glmnet models the "not retained" level, so flip the sign: positive = helps
  read_csv("data/clean/model_coefficients.csv", show_col_types = FALSE) |>
    mutate(stage = "Early-career retention", effect = -estimate),
  read_csv("data/clean/impact_coefficients.csv", show_col_types = FALSE) |>
    mutate(stage = "Impact if retained", effect = estimate)
) |>
  transmute(
    Stage             = stage,
    Feature           = pretty_feature(term),
    Effect            = effect,
    `Absolute Effect` = abs(effect),
    Direction         = if_else(effect > 0, "Positive", "Negative"),
    `Is Draft Pick`   = term == "log_pick"
  )

write_csv(effects, file.path(OUT_DIR, "feature_effects.csv"), na = "")

# ---- 4. Calibration ---------------------------------------------------------
read_csv("data/clean/calibration.csv", show_col_types = FALSE) |>
  transmute(Model            = unname(pretty_model[model]),
            Quintile         = bin,
            Players          = n,
            `Mean Predicted` = mean_predicted,
            `Observed Rate`  = observed_rate) |>
  write_csv(file.path(OUT_DIR, "calibration.csv"), na = "")

# ---- 5. College seasons: player detail view ---------------------------------
dc |>
  select(-player) |>   # drop ESPN's spelling; use the draft name so it matches players.csv
  left_join(select(predictions, draft_year, pick, player), by = c("draft_year", "pick")) |>
  transmute(
    `Draft Year`         = draft_year,
    Pick                 = pick,
    Player               = player,
    Season               = glue("{season - 1}-{str_sub(season, 3, 4)}"),
    `Years Before Draft` = years_before_draft,
    Team                 = team,
    Games                = games,
    MPG                  = mpg,
    `Pts per 40`         = pts_per40,
    `Reb per 40`         = trb_per40,
    `Ast per 40`         = ast_per40,
    `TS%`                = ts_pct,
    Usage                = usage
  ) |>
  write_csv(file.path(OUT_DIR, "college_seasons.csv"), na = "")

# ---- Checks -----------------------------------------------------------------
message(glue("All-Star Game participants found: {nrow(all_stars)} player-years"))
message("Draftees who became All-Stars, with where each model ranked them in their class:")
players |>
  filter(`All-Star`) |>
  select(`Draft Year`, Pick, Player, `All-Star Selections`, `Model Rank in Class`,
         `P(Retained) - Full Model`, `Projected Game Score per 40`) |>
  mutate(across(where(is.double) & !c(`Draft Year`, Pick), ~ round(.x, 2))) |>
  arrange(`Draft Year`, Pick) |>
  print(n = Inf)

message(glue("Tableau files written to {OUT_DIR}/:"))
print(list.files(OUT_DIR))