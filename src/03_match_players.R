# 03_match_players.R — link US college draftees to their college seasons

library(tidyverse)
library(stringdist)
library(glue)

draftees <- read_csv("data/clean/draftee_outcomes.csv", show_col_types = FALSE)
college  <- read_csv("data/raw/college_player_seasons.csv", show_col_types = FALSE)

normalise_name <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z ]", "") |>
    str_squish()
}

normalise_school <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_remove_all("'") |>
    str_replace_all("[^a-z ]", " ") |>
    str_remove_all("\\b(the|university of|university|college)\\b") |>
    str_squish()
}

# Basketball Reference name -> name ESPN used in college (e.g. later married names)
name_aliases <- tribble(
  ~name_key,      ~espn_name_key,
  "megan dileo",             "megan gustafson",
  "meshya williamsholliday", "ameshya williamsholliday"
)

# Draftees the automatic steps can't resolve: add their ESPN college athlete_id here
manual_matches <- tribble(
  ~draft_year, ~pick, ~athlete_id
)

us <- draftees |>
  filter(!international) |>
  transmute(draft_year, pick, player, college,
            name_key   = normalise_name(player),
            school_key = normalise_school(college)) |>
  left_join(name_aliases, by = "name_key") |>
  mutate(name_key = coalesce(espn_name_key, name_key)) |>
  select(-espn_name_key)

roster <- college |>
  distinct(season, athlete_id, espn_name = player, espn_team = team) |>
  mutate(espn_name_key   = normalise_name(espn_name),
         espn_school_key = normalise_school(espn_team))

cands <- us |>
  inner_join(roster, by = c("name_key" = "espn_name_key"), relationship = "many-to-many") |>
  filter(season <= draft_year, season >= draft_year - 1) |>
  mutate(school_match = str_starts(espn_school_key, school_key))

tier1 <- cands |>
  filter(school_match) |>
  group_by(draft_year, pick) |>
  filter(n_distinct(athlete_id) == 1) |>
  summarise(athlete_id = first(athlete_id), espn_team = first(espn_team),
            method = "name + school", .groups = "drop")

tier2 <- cands |>
  anti_join(tier1, by = c("draft_year", "pick")) |>
  group_by(draft_year, pick) |>
  filter(n_distinct(athlete_id) == 1) |>
  summarise(athlete_id = first(athlete_id), espn_team = first(espn_team),
            method = "name only", .groups = "drop")

ambiguous <- cands |>
  anti_join(bind_rows(tier1, tier2), by = c("draft_year", "pick")) |>
  distinct(draft_year, pick, player, college, athlete_id, espn_team)

tier3 <- us |>
  anti_join(bind_rows(tier1, tier2), by = c("draft_year", "pick")) |>
  cross_join(roster) |>
  filter(season <= draft_year, season >= draft_year - 1,
         str_starts(espn_school_key, school_key)) |>
  mutate(dist = stringdist(name_key, espn_name_key, method = "jw")) |>
  group_by(draft_year, pick) |>
  slice_min(dist, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(dist <= 0.15) |>
  transmute(draft_year, pick, athlete_id, espn_team,
            method = glue("fuzzy: {espn_name} ({round(dist, 2)})"))

matches <- bind_rows(
  mutate(manual_matches, method = "manual"),
  tier1, tier2, tier3
) |>
  distinct(draft_year, pick, .keep_all = TRUE)

match_log <- us |>
  select(draft_year, pick, player, college) |>
  left_join(matches, by = c("draft_year", "pick"))

write_csv(match_log, "data/clean/draftee_match_log.csv")

draftee_college <- matches |>
  select(draft_year, pick, athlete_id, match_method = method) |>
  inner_join(college, by = "athlete_id", relationship = "many-to-many") |>
  filter(season <= draft_year) |>
  mutate(years_before_draft = draft_year - season) |>
  arrange(draft_year, pick, season)

write_csv(draftee_college, "data/clean/draftee_college_seasons.csv")

match_log |>
  mutate(method = str_remove(coalesce(method, "unmatched"), ":.*")) |>
  count(method)

message("Unmatched US college draftees:")
match_log |>
  filter(is.na(athlete_id)) |>
  print(n = Inf)

if (nrow(ambiguous) > 0) {
  message("Ambiguous (same name, more than one candidate):")
  print(ambiguous, n = Inf)
}

message("Check these by eye - matched without the school confirming it:")
match_log |>
  filter(method == "name only" | str_starts(method, "fuzzy")) |>
  select(draft_year, pick, player, college, espn_team, method) |>
  print(n = Inf)

draftee_college |>
  group_by(draft_year, pick) |>
  summarise(college_seasons = n(), .groups = "drop") |>
  group_by(draft_year) |>
  summarise(draftees = n(), median_college_seasons = median(college_seasons))

