# LIBRARIES ----
library(tidyverse)

# FUNCTIONS ----
## import function ----
import <- function(df) {
  dat <- df |> 
    mutate(Data = map(.x = file,
                      .f = ~ read_csv(.,
                                      lazy = FALSE, comment = "#",
                                      col_types = cols(Week = col_integer(),
                                                       Kickoff = col_character(),
                                                       Home = col_character(),
                                                       Guest = col_character(),
                                                       Pts_H = col_integer(),
                                                       Pts_G = col_integer())) |>
                        mutate(Kickoff = as.POSIXct(Kickoff)))) |> 
    relocate(Season)
  
    return(dat)
}

## DATA ----
data_raw <- dir(pattern = "\\d{4}.*_[A-z]{3,5}\\.txt", recursive = TRUE) |> 
  enframe(name = NULL, value = "file") |>
  mutate(Season = as.integer(str_extract(file, "\\d{4}"))) |> 
  import()

# RESPONSE ----
cat("\033[1;36m..AFiE >\033[0m object \033[33mdata_raw\033[0m imported \033[1;92m✔\033[0m\n")

# CLEAN UP ----
rm(import)
