## code to prepare `transmission_subnational_level` dataset goes here

library(tidyverse)

# Read weekly transmission data from ecdc COVID dashboard
transmission_subnational_level <- read_csv(here::here("data-raw/subnational level of transmission-2.CSV")) %>%
  select(-5)


usethis::use_data(transmission_subnational_level, overwrite = TRUE)
