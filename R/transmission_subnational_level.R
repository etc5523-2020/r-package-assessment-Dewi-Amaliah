#' Weekly COVID-19 Dominant Source of Transmission in Europe by Subnational Level
#' 
#' A dataset containing the dominant source of transmission of COVID-19 by 
#' subnational level of 32 countries in Europe weekly.
#' 
#' 
#' @format a tibble with 11583 observations and 4 variables.
#' 
#' - **Country**: name of the country
#' - **Region**: subnational level, for example Province
#' - **Level of Transmission**: dominant source of transmission 
#' 
#' + Community transmission
#' + Clusters of cases
#' + Sporadic cases
#' + Not reported i.e. the region did not reported the source of transmission
#' 
#' - **Week**: week from week 1 of 2020 to week 39 of 2020
#' 
#' @import tibble
#' 
#' @source [European Centre of Disease Control and Prevention](https://qap.ecdc.europa.eu/public/extensions/covid-19/covid-19-mobile.html#subnational-transmission-tab)

"transmission_subnational_level"