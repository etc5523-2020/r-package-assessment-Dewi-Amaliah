## code to prepare `country_centroids` dataset goes here

library(readr)
country_centroids <- read_csv("data-raw/country_centroids.csv") %>%
  select(name_long, 
         Longitude,
         Latitude)

usethis::use_data(country_centroids, overwrite = TRUE)
