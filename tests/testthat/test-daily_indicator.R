library(dplyr)


covid_daily <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE))) %>%
  rename(date = X1,
         country = X2,
         population = X3,
         daily_cases = X4,
         daily_deaths = X5,
         daily_cases_rate = X6,
         daily_deaths_rate = X7)

daily_cases <- daily_indicator(covid_daily, indicator = "Cases")
daily_deaths <- daily_indicator(covid_daily, indicator = "Deaths")
daily_deaths_rate <- daily_indicator(covid_daily, indicator = "Deaths per 10,000 people")
daily_cases_rate <- daily_indicator(covid_daily, indicator = "Cases per 10,000 people")

test_that("daily_indicator returns correct variables", {
  expect_equal(names(daily_cases),
               c("country", "date", "count"))
  expect_equal(names(daily_deaths),
               c("country", "date", "count"))
  expect_equal(names(daily_cases_rate),
               c("country", "date", "count"))
  expect_equal(names(daily_deaths_rate),
               c("country", "date", "count"))
  
})

test_that("daily_indicator returns correct dimension", {
  expect_equal(dim(daily_cases),
               c(1000, 3))
  expect_equal(dim(daily_deaths),
               c(1000, 3))
  expect_equal(dim(daily_cases_rate),
               c(1000, 3))
  expect_equal(dim(daily_deaths_rate),
               c(1000, 3))
})




