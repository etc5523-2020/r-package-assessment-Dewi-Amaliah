globalVariables("count")
#' Create A Line Interactive Plot of COVID-19 Variables by Date and Country
#' 
#' This function aims to create a line plot to of COVID-19 variables by date and country.
#' This function could be used to visualize the data resulted from 
#' `daily_indicator` or `cumulative_indicator` functions.
#' 
#' @param data_frame A data frame to be visualized. 
#' 
#' @examples 
#' \dontrun{
#' getplot(daily_deaths)
#' }
#' 
#' @export
getplot <- function(data_frame){
  plot <- ggplot2::ggplot(data_frame, ggplot2::aes(date, count, colour = country)) +
    ggplot2::geom_line(size = 0.3) + ggplot2::labs(x = "Date", y = "Count",
                                 caption = "Data source: European Centre for Disease Prevention and Control") +
    ggplot2::theme_classic() 
  plotly::ggplotly(plot) 
}