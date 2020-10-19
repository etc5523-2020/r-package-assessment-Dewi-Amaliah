#' Explore COVID-19 related variables interactively
#' 
#' A shiny based web app that allows the user to explore the data of COVID-19 related variables in Europe.
#' The users are allowed to customize their country of choices and the time range they want to display within the app. 
#' 
#' The variables covered in this app are:
#' - Daily and cumulative cases and deaths of COVID-19 in 51 countries in Europe.
#' - Weekly testing and positivity rate of 29 EU/EEA member countries and the United Kingdom.
#' - Weekly dominant source of transmission of 31 EU/EEA member countries and the United Kingdom.
#' - Community mobility in six types of location of 29 EU/EEA member countries and the United Kingdom. 
#' 
#' @source The app's source of data is the [European Centre of Disease Prevention and Control](https://www.ecdc.europa.eu/en) 
#' and the [Google Community Mobility Report](https://www.google.com/covid19/mobility/). 
#' The cumulative figures and GCMR are downloaded with the [tidycovid19](https://github.com/joachim-gassen/tidycovid19) R package.
#' 
#' @examples 
#' \dontrun{
#'    launch_app()
#' 
#' }
#' 
#' @export 

launch_app <- function(){
  appDir <- system.file("app", package = "c19euxplorer")
  shiny::runApp(appDir, display.mode = "normal")
}
