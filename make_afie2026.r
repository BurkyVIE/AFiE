# LIBRARIES ----
library(tidyverse)

# SOURCEN ----
source("standings.r")

# DATA ----
## afle_conf_2026 ----
afle_conf_2026 <- teaminfo |>
  filter(Season == 2026, League == "AFLE") |>
  select(Team, Conference)

## afie_data_2026 ----
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

# SAVE RDS-Files to afie2026 folder ----
# saveRDS(afle_conf_2026, "afie2026/afle_conf_2026.rds")
# saveRDS(afie_data_2026, "afie2026/afie_data_2026.rds")

# SAVE TXT-Files to afie2026 folder (Dropbox)
write.table(afle_conf_2026, 
            file = "C:/Users/thoma/Dropbox/shiny_container/afle_conf_2026.txt", 
            sep = ";", 
            row.names = FALSE, 
            fileEncoding = "UTF-8")

write.table(afie_data_2026, 
            file = "C:/Users/thoma/Dropbox/shiny_container/afie_data_2026.txt", 
            sep = ";", 
            row.names = FALSE, 
            fileEncoding = "UTF-8")

# CLEAN UP ----
rm(afle_conf_2026, afie_data_2026)

# RESPONSE ----
cat("\033[1;36m..AFiE >\033[0m rds-files for \033[33mshiny-app (afie2026)\033[0m generated \033[1;92m✔\033[0m\n")
