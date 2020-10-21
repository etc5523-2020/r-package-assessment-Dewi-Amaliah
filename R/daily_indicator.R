globalVariables("country")
globalVariables("daily_cases")
globalVariables("daily_cases_rate")
globalVariables("daily_deaths")
globalVariables("daily_deaths_rate")

#' A Function to Simplify the Daily Cases Visualization Code
#' 
#' This function aims to simplify the code in `c19euxplorer` shiny app by selecting 
#' country, date, and the indicator in daily data to be visualized in Cases and Deaths Tab. 
#' This function would be used in the app.R file. 
#' 
#' You can also use this function outside the app. 
#' However make sure to have these variables in your data frame in order to make the function works.
#' - **daily_cases**
#' - **daily_deaths**
#' - **daily_cases_rate**
#' - **daily_deaths_rate**
#' - **country**
#' - **date**
#' 
#' @param df A daily COVID-19 tibble.
#' @param indicator A shiny's input_id of indicator to be visualized
#' 
#' @return A data frame containing the country, date, and the indicator.
#' 
#' @examples 
#' \dontrun{
#'    daily_indicator(covid_eu_daily, input$indicator_select)
#' }
#' 
#' @export
daily_indicator <- function(df, indicator = c("Cases", "Cases per 10,000 people",
                                              "Deaths", "Deaths per 10,000 people")){
  
  if (indicator == "Cases") {
    dat <- df %>%
      dplyr::select(country, date, daily_cases) %>%
      dplyr::rename(count = daily_cases) 
  }
  
  if (indicator == "Cases per 10,000 people") {
    dat <- df %>%
      dplyr::select(country, date, daily_cases_rate) %>%
      dplyr::rename(count = daily_cases_rate) 
  }
  
  if (indicator =="Deaths") {
    dat <- df %>%
      dplyr::select(country, date, daily_deaths) %>%
      dplyr::rename(count = daily_deaths) 
  }
  
  if (indicator =="Deaths per 10,000 people") {
    dat <- df %>%
      dplyr::select(country, date, daily_deaths_rate) %>%
      dplyr::rename(count = daily_deaths_rate) 
  }
  return(dat)
}