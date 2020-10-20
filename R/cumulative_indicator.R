globalVariables("country")
globalVariables("cumulative_cases")
globalVariables("cumulative_cases_rate")
globalVariables("cumulative_deaths")
globalVariables("cumulative_deaths_rate")

#' A Function to Simplify the Cumulative Cases Visualization Code
#' 
#' This function aims to simplify the code in `c19euxplorer` shiny app by selecting the cumulative data
#' and indicator to visualized in Cases and Deaths Tab. 
#' This function would be used in the app.R file. 
#' 
#' @param df A cumulative COVID-19 tibble.
#' @param indicator A shiny's input_id of indicator to be visualized.
#' 
#' @return A data frame containing the country, date, and the indicator.
#' 
#' @examples 
#' \dontrun{
#' cumulative_indicator(covid_eu_cumulative, input$indicator_select)
#' }
#' 
#' @export
cumulative_indicator <- function(df, indicator){
  if (indicator == "Cases") {
    dat <- df %>%
      dplyr::select(country, date, cumulative_cases) %>%
      dplyr::rename(count = cumulative_cases) 
  }
  
  if (indicator == "Cases per 10,000 people") {
    dat <- df %>%
      dplyr::select(country, date, cumulative_cases_rate) %>%
      dplyr::rename(count = cumulative_cases_rate) 
  }
  
  if (indicator =="Deaths") {
    dat <- df %>%
      dplyr::select(country, date, cumulative_deaths) %>%
      dplyr::rename(count = cumulative_deaths) 
  }
  
  if (indicator =="Deaths per 10,000 people") {
    dat <- df %>%
      dplyr::select(country, date, cumulative_deaths_rate) %>%
      dplyr::rename(count = cumulative_deaths_rate) 
  }
  return(dat)
}