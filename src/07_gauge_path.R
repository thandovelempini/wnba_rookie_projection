library(tidyverse)

players <- read_csv("data/tableau/players.csv", show_col_types = FALSE)
n <- 100

ring <- function(col, radius, name) {
  players |>
    transmute(`Draft Year`, Pick,
              p = if (is.null(col)) 1 else .data[[col]]) |>
    filter(!is.na(p)) |>
    crossing(Point = 0:n) |>
    mutate(Ring  = name,
           theta = 2 * pi * p * Point / n,
           X     = radius * sin(theta),
           Y     = radius * cos(theta)) |>
    select(-p, -theta)
}

bind_rows(
  ring(NULL, 1.0, "Model track"),
  ring(NULL, 0.8, "Slot track"),
  ring("P(Retained) - Full Model", 1.0, "Full model"),
  ring("P(Retained) - Draft Slot", 0.8, "Draft slot")
) |>
  write_csv("data/tableau/gauge_path.csv")

