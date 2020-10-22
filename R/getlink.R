#' A Simpler Way To Create Hyperlink Text in Shiny App
#' 
#' This function aims to simplify the typing of code to create hyperlink text in a Shiny app. 
#' It is useful especially when there are a lot of hyperlink texts to be typed in a Shiny app's UI.
#' 
#' @param url A valid URL in string 
#' @param label A string to label the URL
#' 
#' @examples 
#' getlink("https://datain360.com", "Dewi's web")
#' 
#' @export
getlink <- function(url, label){

  shiny::tags$a(href = url,
                label,
                class = "externallink")
  
}