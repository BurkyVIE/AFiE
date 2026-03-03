# LIBRARIES ----
library(tidyverse)

# SOURCEN ----
source("teams.r")
source("import.r")

# DATA ----
## games ----
games <- data_raw |> 
  select(-file) |> 
  unnest_longer(Data) |> # full list of game data
  unpack(Data) |> 
  left_join(teaminfo, by = c("Guest" = "Team", "Season")) |> # enrich team info
  nest(Guestdata = Franchise:Division) |> 
  left_join(teaminfo, by = c("Home" = "Team", "Season")) |> 
  nest(Homedata = Franchise:Division) |>
  rowwise() |> # rowwise wg Season und Week
  ungroup() |> 
  arrange(Season, Week, !is.na(Kickoff), Kickoff) #arranging by the logical vector !is.na(wt) of TRUE/FALSE (= F < T)

# RESPONSE ----
cat("\033[1;36m..AFiE >\033[0m  ⤷ object \033[33mgames\033[0m generated \033[1;92m✔\033[0m\n")

## results ----
results <- bind_rows(
  games |> rename(Team = Guest, Opponent = Home, PF = Pts_G, PA = Pts_H, Teamdata = Guestdata, Oppdata = Homedata) |> add_column(Home = FALSE),
  games |> rename(Team = Home, Opponent = Guest, PF = Pts_H, PA = Pts_G, Teamdata = Homedata, Oppdata = Guestdata) |> add_column(Home = TRUE)) |>
  filter(!is.na(PF)) |> 
  mutate(Result = case_when(PF > PA ~ "W",
                            PF < PA ~ "L",
                            PF == PA ~ "T",
                            TRUE ~ NA_character_)) |> 
  relocate(Result, Home, .after = "PA") |> 
  arrange(Season, Week, !is.na(Kickoff), Kickoff) #arranging by the logical vector !is.na(wt) of TRUE/FALSE (= F < T)

### 2023 season Leipzig Kings folded after week 5 - games @/vs Cologne Centurions score 16-16 counted as W for Cologne
results <- rows_update(results, tibble(Season = 2023, Team = "Cologne Centurions", Week = c(7, 12), Result = "W"), by = c("Season", "Team", "Week"))
results <- rows_update(results, tibble(Season = 2023, Team = "Leipzig Kings", Week = c(7, 12), Result = "L"), by = c("Season", "Team", "Week"))
### Following the Leipzig Kings folding in week 12 Prague gave the home right to Fehervar, who otherwise may have lost two home games
### -> this only applies to the location the game was held, for statistics reasons nothing changed

# CLEAN UP ----

# RESPONSE ----
cat("\033[1;36m..AFiE >\033[0m  ⤷ object \033[33mresults\033[0m generated \033[1;92m✔\033[0m\n")

