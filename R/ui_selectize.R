#' Refactor User Interface `selectizeInput` Function
#' 
#' This function aim to simplify the repetitive call of shiny's selectizeInput function. 
#' It will be called in app.R file.
#' 
#' @param id Input id that will be used in the server side.
#' @param choices List of value to select. 
#' 
#' @examples 
#' ui_selectize("country_select", c("Italy", "Spain", "France", "Austria", "United Kingdom"))
#' 
#' @export
ui_selectize <- function(id, choices){
  shiny::selectizeInput(id, 
                 label = "Step 1: Select or type at least 1 country",
                 choices,
                 selected = c("Italy", "Austria", "Spain"),
                 multiple = TRUE)
}