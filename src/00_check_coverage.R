# =============================================================================
# check_coverage.R
# WNBA Rookie Projection — step 0: check data coverage before committing
#
# What this does:
#   1. Tests how far back wehoop's women's college (WBB) player box scores go
#   2. Pulls two WNBA draft classes from Basketball Reference (early + recent)
#   3. Takes a first look at how messy college <-> WNBA name matching will be
#
# Note: wehoop seasons are labelled by the year the season ENDS,
#       so season = 2024 means the 2023-24 college season.
# =============================================================================

library(arrow)
library(wehoop)
library(tidyverse)
library(janitor)
library(rvest)
library(glue)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. College coverage check
# -----------------------------------------------------------------------------
# A spread of seasons: the earliest we'd need (2007 = 2006-07),
# the start of our window, and a few later ones.
test_seasons <- c(2007, 2010, 2015, 2020, 2024)

check_wbb_season <- function(season) {
  message(glue("Checking college season {season}..."))
  
  box <- tryCatch(
    wehoop::load_wbb_player_box(seasons = season),
    error = function(e) {
      message(glue("  -> failed: {conditionMessage(e)}"))
      NULL
    }
  )
  
  if (is.null(box) || nrow(box) == 0) {
    return(tibble(season = season, rows = 0L, players = NA_integer_,
                  games = NA_integer_, minutes_missing_pct = NA_real_,
                  turnovers_missing_pct = NA_real_))
  }
  
  box <- clean_names(box)
  
  pct_missing <- function(col) {
    if (col %in% names(box)) round(mean(is.na(box[[col]])) * 100, 1) else NA_real_
  }
  
  write_csv(box, glue("data/raw/wbb_player_box_{season}.csv"))
  
  # Rows for players who didn't play have no minutes
  played <- if ("did_not_play" %in% names(box)) filter(box, !did_not_play %in% TRUE) else box
  
  tibble(
    season                     = season,
    rows                       = nrow(box),
    players                    = n_distinct(box$athlete_id),
    games                      = n_distinct(box$game_id),
    minutes_missing_pct        = pct_missing("minutes"),
    minutes_missing_played_pct = if ("minutes" %in% names(played))
      round(mean(is.na(played$minutes)) * 100, 1) else NA_real_,
    turnovers_missing_pct      = pct_missing("turnovers")
  )
}

coverage <- map_dfr(test_seasons, check_wbb_season)
print(coverage)

# What to look for:
#   - rows = 0 for early seasons  -> wehoop doesn't reach back that far;
#                                    we'll use Sports Reference totals there
#   - far fewer games in early seasons -> partial coverage (e.g. only some
#                                    conferences), also a problem for us
#   - high % missing minutes/turnovers -> per-40 stats won't be reliable

# -----------------------------------------------------------------------------
# 2. WNBA draft classes from Basketball Reference
# -----------------------------------------------------------------------------
# Basketball Reference tables have a two-row header, so we find the row
# that holds the real column names and use that.

get_draft_class <- function(year) {
  message(glue("Fetching {year} WNBA draft..."))
  Sys.sleep(4)  # be polite: Basketball Reference rate-limits fast scrapers
  
  url  <- glue("https://www.basketball-reference.com/wnba/draft/{year}.html")
  page <- read_html(url)
  
  raw <- html_table(page, header = FALSE)[[1]]
  
  header_row <- which(apply(raw, 1, function(r) "Player" %in% r))[1]
  names(raw) <- make.unique(as.character(unlist(raw[header_row, ])))
  
  draft <- raw[-(1:header_row), ] |>
    clean_names() |>
    select(-starts_with("na")) |>                 # empty spacer columns
    filter(str_detect(pk, "^\\d+$")) |>           # drops "Round 2"/"Round 3" and repeated headers
    mutate(across(c(pk, yrs, g, mp, pts, trb, ast, ws, ws_40), ~ suppressWarnings(as.numeric(.x))),
           college    = na_if(college, ""),
           draft_year = year)
  
  write_csv(draft, glue("data/raw/wnba_draft_{year}.csv"))
  draft
}

draft_2010 <- get_draft_class(2010)
draft_2024 <- get_draft_class(2024)

glimpse(draft_2024)

# -----------------------------------------------------------------------------
# 3. First look at name matching
# -----------------------------------------------------------------------------
# How many 2024 draftees appear by exact name in the 2023-24 college data?

if (file.exists("data/raw/wbb_player_box_2024.csv")) {
  college_2024_names <- read_csv("data/raw/wbb_player_box_2024.csv",
                                 show_col_types = FALSE) |>
    distinct(athlete_display_name) |>
    pull()
  
  # Normalise names: strip accents (Mühl -> Muhl), lowercase, drop punctuation
  normalise_name <- function(x) {
    x |>
      stringi::stri_trans_general("Latin-ASCII") |>
      str_to_lower() |>
      str_replace_all("[^a-z ]", "") |>
      str_squish()
  }
  
  name_check <- draft_2024 |>
    filter(!is.na(college)) |>                    # international draftees have no US college
    select(player, college, pk) |>
    mutate(exact_match = normalise_name(player) %in% normalise_name(college_2024_names))
  
  message(glue("Exact name matches: {sum(name_check$exact_match)} of {nrow(name_check)}"))
  print(filter(name_check, !exact_match))
  # Unmatched players are usually: international players (no US college),
  # accents/apostrophes, nicknames, or players whose last college season
  # was earlier than 2023-24.
}
