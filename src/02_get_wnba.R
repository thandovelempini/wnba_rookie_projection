# 02_get_wnba.R — WNBA draft classes and early-career outcomes

library(arrow)
library(wehoop)
library(tidyverse)
library(janitor)
library(rvest)
library(glue)

DRAFT_YEARS <- 2018:2026
LAST_SEASON <- 2026
WINDOW      <- 3   # draft-year season plus the two that follow
# retained = played in season 2 or 3 of that window (early-career retention)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

normalise_name <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z ]", "") |>
    str_squish()
}

get_draft_class <- function(year) {
  message(glue("Fetching {year} WNBA draft..."))
  Sys.sleep(4)
  
  page <- read_html(glue("https://www.basketball-reference.com/wnba/draft/{year}.html"))
  raw  <- html_table(page, header = FALSE)[[1]]
  
  header_row <- which(apply(raw, 1, function(r) "Player" %in% r))[1]
  hdr <- as.character(unlist(raw[header_row, ]))
  hdr[is.na(hdr) | hdr == ""] <- paste0("blank_", seq_len(sum(is.na(hdr) | hdr == "")))
  names(raw) <- make.unique(hdr)
  
  raw[-(1:header_row), ] |>
    clean_names() |>
    select(-starts_with("blank")) |>
    mutate(round = 1 + cumsum(str_detect(pk, "^Round"))) |>   # actual round from the "Round N" divider rows
    filter(str_detect(pk, "^\\d+$")) |>
    mutate(across(any_of(c("pk", "yrs", "g", "mp", "pts", "trb", "ast", "ws", "ws_40")),
                  ~ suppressWarnings(as.numeric(.x))),
           draft_year = year)
}

drafts_raw <- map_dfr(DRAFT_YEARS, get_draft_class)
write_csv(drafts_raw, "data/raw/wnba_drafts.csv")

drafts <- drafts_raw |>
  transmute(
    draft_year,
    pick          = pk,
    round,
    draft_team    = team,
    player,
    college       = na_if(college, ""),
    country       = na_if(former, ""),
    international = is.na(college),
    br_career_yrs = yrs,
    name_key      = normalise_name(player)
  )

wnba <- load_wnba_player_box(seasons = DRAFT_YEARS) |>
  clean_names()

if ("season_type" %in% names(wnba)) wnba <- filter(wnba, season_type == 2)

wnba_seasons <- wnba |>
  filter(!did_not_play %in% TRUE, !is.na(minutes), minutes > 0) |>
  mutate(name_key = normalise_name(athlete_display_name)) |>
  group_by(season, athlete_id, name_key) |>
  summarise(
    games   = n_distinct(game_id),
    minutes = sum(minutes),
    pts     = sum(points),
    gmsc    = sum(points + 0.4 * field_goals_made - 0.7 * field_goals_attempted -
                    0.4 * (free_throws_attempted - free_throws_made) +
                    0.7 * offensive_rebounds + 0.3 * defensive_rebounds +
                    steals + 0.7 * assists + 0.7 * blocks - 0.4 * fouls - turnovers),
    .groups = "drop"
  )

# Basketball Reference name -> ESPN name, for names that differ beyond accents
name_aliases <- tribble(
  ~name_key,         ~espn_name_key,
  "anastasiia kosu", "anastasiia olairi kosu"
)

# Names are used only to find each draftee's ESPN athlete ID (searching all
# seasons); seasons are then pulled by ID, so mid-career name changes still match
draft_ids <- drafts |>
  left_join(name_aliases, by = "name_key") |>
  mutate(match_key = coalesce(espn_name_key, name_key)) |>
  inner_join(distinct(wnba_seasons, athlete_id, name_key),
             by = c("match_key" = "name_key"), relationship = "many-to-many") |>
  distinct(draft_year, pick, player, athlete_id)

ambiguous <- draft_ids |>
  group_by(draft_year, pick) |>
  filter(n_distinct(athlete_id) > 1) |>
  ungroup()

# Picks checked by hand: several ESPN IDs that are the same player (seasons don't overlap)
verified_multi_id <- tribble(
  ~draft_year, ~pick, ~note,
  2019,        35,    "Li Yueru: duplicate ESPN records, 2022 and 2024-26"
)

unverified <- anti_join(ambiguous, verified_multi_id, by = c("draft_year", "pick"))
if (nrow(unverified) > 0) {
  print(unverified, n = Inf)
  stop("Unverified multi-ID matches above. Check their seasons; if each is one player, ",
       "add the pick to verified_multi_id, otherwise add a name alias.", call. = FALSE)
}

window_stats <- draft_ids |>
  inner_join(select(wnba_seasons, -name_key), by = "athlete_id",
             relationship = "many-to-many") |>
  filter(season >= draft_year, season < draft_year + WINDOW) |>
  mutate(yr = season - draft_year + 1)

outcomes <- window_stats |>
  group_by(draft_year, pick) |>
  summarise(
    seasons_played  = n_distinct(season),
    played_y1       = any(yr == 1),
    played_y2       = any(yr == 2),
    played_y3       = any(yr == 3),
    games           = sum(games),
    minutes         = sum(minutes),
    pts             = sum(pts),
    gmsc            = sum(gmsc),
    .groups         = "drop"
  )

first_season <- wnba_seasons |>
  group_by(athlete_id) |>
  summarise(first_wnba_season = min(season))

draftees <- drafts |>
  left_join(draft_ids |>
              group_by(draft_year, pick) |>
              summarise(wnba_athlete_id = first(athlete_id), .groups = "drop"),
            by = c("draft_year", "pick")) |>
  left_join(first_season, by = c("wnba_athlete_id" = "athlete_id")) |>
  left_join(outcomes, by = c("draft_year", "pick")) |>
  mutate(
    across(c(seasons_played, games, minutes, pts, gmsc), ~ replace_na(.x, 0)),
    across(starts_with("played_"), ~ replace_na(.x, FALSE)),
    seasons_available = pmin(WINDOW, LAST_SEASON - draft_year + 1),
    window_complete   = seasons_available == WINDOW,
    retained       = if_else(window_complete, played_y2 | played_y3, NA),
    mpg               = if_else(games > 0, minutes / games, NA_real_),
    pts_per40         = if_else(minutes > 0, pts / minutes * 40, NA_real_),
    gmsc_per40        = if_else(minutes > 0, gmsc / minutes * 40, NA_real_)
  )

write_csv(draftees, "data/clean/draftee_outcomes.csv")

draftees |>
  group_by(draft_year) |>
  summarise(picks         = n(),
            international = sum(international),
            played_any    = sum(seasons_played > 0),
            retained   = sum(retained, na.rm = TRUE),
            window_done   = first(window_complete))

message("Name-match failures (Basketball Reference shows WNBA games, no ESPN ID found):")
draftees |>
  filter(br_career_yrs > 0, is.na(wnba_athlete_id)) |>
  select(draft_year, pick, player, college, br_career_yrs) |>
  print(n = Inf)

message("Late debuts (first WNBA game after the three-season window):")
draftees |>
  filter(!is.na(first_wnba_season), first_wnba_season >= draft_year + WINDOW) |>
  select(draft_year, pick, player, college, first_wnba_season) |>
  print(n = Inf)

if (nrow(ambiguous) > 0) {
  message("Names matching more than one WNBA player:")
  print(ambiguous, n = Inf)
}