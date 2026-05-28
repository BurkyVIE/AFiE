library(tidyverse)

afie_data_2026 <- data_raw |>
  filter(Season == 2026)|>
  transmute(Liga = str_extract(file, "AFLE|EFA"), Data) |>
  unnest_longer(Data) |> unpack(Data) |>
  arrange(Kickoff) |>
  left_join(teaminfo |>
              filter(Season == 2026) |>
              select(Team, League),
            by = c("Home" = "Team")) |>
  transmute(Liga,
            Phase = case_when(Week == 98 ~ "Semifinale",
                              Week == 99 ~ "Finale",
                              TRUE ~ "Grunddurchgang"),
            Datum = as_date(Kickoff),
            Zeit = strftime(Kickoff, format = "%H:%M"),
            Zeit = case_when(Zeit == "00:00" ~ " TBD ",
                             TRUE ~ Zeit),
            Gast = Guest,
            Heim = Home,
            Score_Gast = Pts_G,
            Score_Heim = Pts_H)

saveRDS(afie_data_2026, "afie2026/afie_data_2026.rds")