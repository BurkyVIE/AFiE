# LIBRARIES ----
library(tidyverse)

# SOURCEN ----
source("games_results.r")

# FUNCTION ----
standings_fn <- function(season, league){

  # Set for calculation
  set <- results |> 
    unnest(Teamdata) |> 
    filter(Season == season,
           League == league,
           Wk < 20)
  
  make_stand <- function(data, Teams = NULL)

    {
  
    if(!is.null(Teams)) data <- filter(data, Team %in% Teams, Opponent %in% Teams)
      data |> group_by(Team) |> 
          summarise(Gs = n(), PF = sum(PF), PA = sum(PA), W = sum(Result == "W"), L = sum(Result == "L"), T = sum(Result == "T")) |> 
      mutate(Pct = (W + .5*T) / Gs) |> 
      arrange(desc(Pct), desc(PF - PA)) |> 
      mutate(Rank = rank(desc(Pct), ties.method = "average"))
    }

  # Standings 1st iteration
  tab1 <- make_stand(set)
  
  # List of Temas with identical Pct > H2H
  h2h <- tab1 |>
    group_by(Pct) |>
    summarise(Ts = n(), Teams = list(Team)) |>
    filter(Ts > 1) |>
    pull(Teams)
  
  # Standings 2st iteration (for H2H)
  tab2 <- map(h2h, ~make_stand(set, .)) |>
    bind_rows()
  
  # Bind 1st and 2nd iteration
  left_join(select(tab1, -Rank), select(tab2, Team, h2h = Rank), by = "Team") |> 
    mutate(PDiff = PF - PA) |> 
    arrange(desc(Pct), h2h, desc(PDiff)) -> tab
    
  # Clean up
  rm(set, tab1, h2h, tab2)
  
  # return
  return(tab)
  
}

# all season leage combos
comb <- select(teaminfo, Season, League) |>
  unique() |>
  arrange(Season) |>
  tail(-2) |> # 1991 & 1992 Season
  head(-2) # 2026 AFLE & EFA Season


standings <- mutate(comb, data = comb |> map2(.x = Season, .y = League, .f = ~standings_fn(.x, .y))) |>
  unnest_longer(data) |>
  unpack(data) #|>
  # group_split(Season, League)
  
# CLEAN UP ----
rm(comb)

# RESPONSE ----
cat("\033[1;36m..AFiE >\033[0m  ⤷ object \033[33mstandings\033[0m generated (NOT 2026) \033[1;92m✔\033[0m\n")
