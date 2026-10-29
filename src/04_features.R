# 04_features.R — model-ready table: one row per US college draftee

library(arrow)
library(wehoop)
library(tidyverse)
library(glue)

MIN_REF_MINUTES <- 300   # reference population for standardising stats within each season

college  <- read_csv("data/raw/college_player_seasons.csv", show_col_types = FALSE)
dc       <- read_csv("data/clean/draftee_college_seasons.csv", show_col_types = FALSE)
outcomes <- read_csv("data/clean/draftee_outcomes.csv", show_col_types = FALSE)

rate_cols <- c("pts_per40", "trb_per40", "orb_per40", "drb_per40", "ast_per40",
               "stl_per40", "blk_per40", "tov_per40", "ts_pct", "efg_pct",
               "fg3a_rate", "ft_rate", "ft_pct", "ast_tov", "usage")

# ---- Season-standardised stats (z-scores vs D1 players with 300+ minutes) ----
# Fitted globally, before cross-validation: each season's mean and sd come from all
# D1 players that season (not draftees, no outcomes). That information exists before
# the draft, so it isn't leakage, but it is not refitted inside each CV fold.
# Imputation and normalisation in model_utils.R ARE refitted inside each fold.
ref <- college |>
  filter(minutes >= MIN_REF_MINUTES) |>
  group_by(season) |>
  summarise(across(all_of(rate_cols),
                   list(mu = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))),
            .groups = "drop")

standardise <- function(df) {
  df <- left_join(df, ref, by = "season")
  for (col in rate_cols) {
    df[[paste0(col, "_z")]] <- (df[[col]] - df[[paste0(col, "_mu")]]) / df[[paste0(col, "_sd")]]
  }
  select(df, -ends_with("_mu"), -ends_with("_sd"))
}

# ---- Team strength: simple rating system from game margins ------------------
team_strength <- tryCatch({
  sched <- load_wbb_schedule(seasons = sort(unique(college$season)))
  
  team_games <- bind_rows(
    sched |> transmute(season, team_id = as.character(home_id), opp_id = as.character(away_id),
                       margin = as.numeric(home_score) - as.numeric(away_score)),
    sched |> transmute(season, team_id = as.character(away_id), opp_id = as.character(home_id),
                       margin = as.numeric(away_score) - as.numeric(home_score))
  ) |>
    filter(!is.na(margin))
  
  d1 <- team_games |>
    count(season, team_id) |>
    filter(n >= 15) |>
    select(season, team_id)
  
  team_games <- team_games |>
    semi_join(d1, by = c("season", "team_id")) |>
    semi_join(rename(d1, opp_id = team_id), by = c("season", "opp_id"))
  
  team_margin <- team_games |>
    group_by(season, team_id) |>
    summarise(margin = mean(margin), .groups = "drop")
  
  srs <- mutate(team_margin, sos = 0, srs = margin)
  for (i in 1:15) {
    sos <- team_games |>
      inner_join(select(srs, season, opp_id = team_id, opp_srs = srs), by = c("season", "opp_id")) |>
      group_by(season, team_id) |>
      summarise(sos = mean(opp_srs), .groups = "drop")
    srs <- team_margin |>
      left_join(sos, by = c("season", "team_id")) |>
      mutate(sos = replace_na(sos, 0), srs = margin + sos) |>
      group_by(season) |>
      mutate(srs = srs - mean(srs)) |>
      ungroup()
  }
  
  # Team-level, opponent-adjusted: team_schedule_strength is the average rating of
  # the team's opponents over the season. It is not player-specific (it doesn't
  # account for games an individual player missed).
  select(srs, season, team_id, team_srs = srs, team_schedule_strength = sos)
}, error = function(e) {
  message(glue("Team strength skipped: {conditionMessage(e)}"))
  tibble(season = integer(), team_id = character(), team_srs = double(),
         team_schedule_strength = double())
})

# ---- Draftee features -------------------------------------------------------
dc_z <- dc |>
  standardise() |>
  mutate(team_id = as.character(team_id)) |>
  left_join(team_strength, by = c("season", "team_id"))

latest <- dc_z |>
  group_by(draft_year, pick) |>
  slice_max(season, n = 1, with_ties = FALSE) |>
  ungroup()

previous <- dc_z |>
  group_by(draft_year, pick) |>
  arrange(season, .by_group = TRUE) |>
  filter(n() >= 2) |>
  slice(n() - 1) |>
  ungroup() |>
  select(draft_year, pick, prev_pts_per40 = pts_per40, prev_ts_pct = ts_pct,
         prev_usage = usage, prev_mpg = mpg)

career <- dc |>
  group_by(draft_year, pick) |>
  summarise(college_seasons_obs = n(),
            first_obs_season    = min(season),
            career_minutes      = sum(minutes),
            .groups = "drop") |>
  mutate(career_truncated = first_obs_season == min(college$season))

features <- latest |>
  select(draft_year, pick, college_athlete_id = athlete_id, final_season = season,
         final_team = team, position, games, starts, mpg, minutes,
         all_of(rate_cols), ends_with("_z"), team_srs, team_schedule_strength) |>
  mutate(start_share = starts / games, .after = starts) |>
  select(-starts) |>
  left_join(previous, by = c("draft_year", "pick")) |>
  mutate(pts_per40_change = pts_per40 - prev_pts_per40,
         ts_pct_change    = ts_pct - prev_ts_pct,
         usage_change     = usage - prev_usage,
         mpg_change       = mpg - prev_mpg) |>
  select(-starts_with("prev_")) |>
  left_join(career, by = c("draft_year", "pick"))

model_data <- outcomes |>
  filter(!international) |>
  select(draft_year, pick, pick_round = round, player, college, draft_team, retained,
         seasons_played, wnba_games = games, wnba_minutes = minutes,
         wnba_pts_per40 = pts_per40, wnba_gmsc_per40 = gmsc_per40,
         window_complete, first_wnba_season) |>
  inner_join(features, by = c("draft_year", "pick")) |>
  mutate(
    split          = if_else(window_complete, "train", "predict"),
    position_group = case_when(str_detect(position, "C") ~ "C",
                               str_detect(position, "F") ~ "F",
                               str_detect(position, "G") ~ "G",
                               TRUE ~ NA_character_)
  )

write_csv(model_data, "data/clean/model_data.csv")

# ---- Checks -----------------------------------------------------------------
count(model_data, split)

message("Raw ESPN positions -> position group (check any hybrids, e.g. 'G-F'):")
count(model_data, position, position_group) |> print(n = Inf)

message("Picks per actual draft round:")
count(model_data, draft_year, pick_round) |>
  pivot_wider(names_from = pick_round, values_from = n, names_prefix = "round_") |>
  print()

message("Features with missing values:")
model_data |>
  summarise(across(everything(), ~ round(mean(is.na(.x)) * 100, 1))) |>
  pivot_longer(everything(), names_to = "column", values_to = "pct_missing") |>
  filter(pct_missing > 0) |>
  arrange(desc(pct_missing)) |>
  print(n = Inf)

message("Retained (played in season 2 or 3) vs not, training classes:")
model_data |>
  filter(split == "train") |>
  group_by(retained) |>
  summarise(n             = n(),
            median_pick   = median(pick),
            mpg           = round(mean(mpg), 1),
            pts_per40_z   = round(mean(pts_per40_z, na.rm = TRUE), 2),
            ts_pct_z      = round(mean(ts_pct_z, na.rm = TRUE), 2),
            usage_z       = round(mean(usage_z, na.rm = TRUE), 2),
            team_srs      = round(mean(team_srs, na.rm = TRUE), 1))