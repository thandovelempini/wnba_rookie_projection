# get_college.R — women's college player season stats from wehoop (ESPN box scores)

library(arrow)
library(wehoop)
library(tidyverse)
library(janitor)
library(glue)

SEASONS        <- 2017:2026   # 2017 = 2016-17, first season with full coverage
MIN_TEAM_GAMES <- 15          # teams below this are non-D1 opponents

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

box <- load_wbb_player_box(seasons = SEASONS) |>
  clean_names()

played <- box |>
  filter(!did_not_play %in% TRUE, !is.na(minutes), minutes > 0)

d1_teams <- played |>
  group_by(season, team_id) |>
  summarise(team_games = n_distinct(game_id), .groups = "drop") |>
  filter(team_games >= MIN_TEAM_GAMES)

played <- semi_join(played, d1_teams, by = c("season", "team_id"))

team_game <- played |>
  group_by(season, game_id, team_id) |>
  summarise(tm_min = sum(minutes),
            tm_fga = sum(field_goals_attempted),
            tm_fta = sum(free_throws_attempted),
            tm_tov = sum(turnovers),
            .groups = "drop")

college <- played |>
  left_join(team_game, by = c("season", "game_id", "team_id")) |>
  group_by(season, athlete_id) |>
  summarise(
    player   = first(athlete_display_name),
    team     = names(which.max(table(team_display_name))),
    team_id  = names(which.max(table(team_id))),
    position = first(athlete_position_abbreviation),
    games    = n_distinct(game_id),
    starts   = sum(starter %in% TRUE),
    minutes  = sum(minutes),
    pts      = sum(points),
    fgm      = sum(field_goals_made),
    fga      = sum(field_goals_attempted),
    fg3m     = sum(three_point_field_goals_made),
    fg3a     = sum(three_point_field_goals_attempted),
    ftm      = sum(free_throws_made),
    fta      = sum(free_throws_attempted),
    orb      = sum(offensive_rebounds),
    drb      = sum(defensive_rebounds),
    trb      = sum(rebounds),
    ast      = sum(assists),
    stl      = sum(steals),
    blk      = sum(blocks),
    tov      = sum(turnovers),
    pf       = sum(fouls),
    tm_min   = sum(tm_min),
    tm_fga   = sum(tm_fga),
    tm_fta   = sum(tm_fta),
    tm_tov   = sum(tm_tov),
    .groups  = "drop"
  ) |>
  mutate(
    mpg       = minutes / games,
    across(c(pts, trb, orb, drb, ast, stl, blk, tov), ~ .x / minutes * 40, .names = "{.col}_per40"),
    ts_pct    = pts / (2 * (fga + 0.44 * fta)),
    efg_pct   = (fgm + 0.5 * fg3m) / fga,
    fg3a_rate = fg3a / fga,
    ft_rate   = fta / fga,
    ft_pct    = ftm / fta,
    ast_tov   = if_else(tov > 0, ast / tov, NA_real_),
    usage     = 100 * ((fga + 0.44 * fta + tov) * (tm_min / 5)) /
      (minutes * (tm_fga + 0.44 * tm_fta + tm_tov))
  ) |>
  mutate(across(where(is.double), ~ if_else(is.finite(.x), .x, NA_real_))) |>
  select(-starts_with("tm_"))

write_csv(college, "data/raw/college_player_seasons.csv")

message(glue("{nrow(college)} player-seasons, {n_distinct(college$athlete_id)} players"))

college |>
  group_by(season) |>
  summarise(teams          = n_distinct(team_id),
            player_seasons = n(),
            median_games   = median(games),
            median_mpg     = round(median(mpg), 1))

college |>
  filter(season == 2024, minutes >= 500) |>
  arrange(desc(pts_per40)) |>
  select(player, team, games, mpg, pts_per40, ts_pct, usage) |>
  head(10)

