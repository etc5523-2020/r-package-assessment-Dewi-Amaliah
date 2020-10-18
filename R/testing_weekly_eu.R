#' Weekly Europe's COVID-19 Testing and Positivity Rates
#' 
#' A dataset containing weekly COVID-19 testing and positivity positivity rate 
#' of 30 countries in Europe (EU/EEA member countries and UK). 
#' 
#' @format a tibble with 954 observations and 9 variables. 
#' 
#' - **country**: name of the country
#' - **country_code**: 2-letter ISO country code
#' - **year_week**: week of the year in yyyy-ww format
#' - **new_cases**: number of new confirmed cases
#' - **test_done**: number of test done
#' - **population**: 2019 population from the World Bank
#' - **testing_rate**: testing rate per 100,000 population
#' - **positivity_rate**: weekly test positivity (%): 100 x Number of new confirmed cases/number of tests done per week
#' - **testing_data_source**: data source of testing
#' 
#' 
#' 
#' @source [European Centre for Disease Prevention and Control](https://www.ecdc.europa.eu/en/publications-data/covid-19-testing)
#' 
"testing_weekly_eu"