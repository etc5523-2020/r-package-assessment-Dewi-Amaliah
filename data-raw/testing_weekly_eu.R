## code to prepare `testing_weekly_eu` dataset goes here

library(readxl)

testing_weekly_eu <- read_xlsx("data-raw/weekly_testing_2020_10_01.xlsx")

usethis::use_data(testing_weekly_eu, overwrite = TRUE)
