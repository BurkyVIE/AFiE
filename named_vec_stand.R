library(tidyverse)

bdng <- c("Season" = 2026, "League" = "AFLE", "Conference" = "South/East")
# bdng <- c("Season" = 2026)

adr <- imap(bdng, function(wert, spalte) {
  rlang::expr(.data[[!!spalte]] == !!wert)
})

set <- rlang::inject(unnest(results, Teamdata) |> filter(!!!unname(adr))) |> 
  filter(Wk <= 20)

make_stand <- function(data, Teams = NULL) {
  if(!is.null(Teams)) data <- filter(data, Team %in% Teams, Opponent %in% Teams)
  data |>
    summarise(.by = c(Team, Season, League, Conference, Division),
              Gs = n(), PF = sum(PF), PA = sum(PA), W = sum(Result == "W"), L = sum(Result == "L"), T = sum(Result == "T")) |> 
    mutate(Pct = (W + .5*T) / Gs) |> 
    arrange(desc(Pct), desc(PF - PA)) |> 
    mutate(Rank = rank(desc(Pct), ties.method = "average"))
}

# Standings 1st iteration
tab1 <- make_stand(set)

# List of Temas with identical Pct -> H2H
h2h <- tab1 |>
  summarise(Ts = n(), Teams = list(Team), .by = Pct) |>
  filter(Ts > 1) |>
  pull(Teams)

# Standings 2st iteration (for H2H)
tab2 <- map(h2h, ~make_stand(set, .)) |>
  bind_rows()

# Bind 1st and 2nd iteration
tab <- if(length(h2h) == 0) # is there any h2h
  add_column(select(tab1, -Rank), H2H = NA_real_) else 
    left_join(select(tab1, -Rank), select(tab2, Team, H2H = Rank), by = "Team")

tab <- tab |> 
  mutate(PDiff = PF - PA) |> 
  arrange(desc(Pct), H2H, desc(PDiff))

# Clean up
rm(bdng, adr, set, make_stand, tab1, h2h, tab2)
