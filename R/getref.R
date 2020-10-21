#' Create A Reference List in Shiny App
#' 
#' This function aims to create reference in an list format in the Shiny web app through a html tag. 
#' 
#' @param author The author/authors in string.
#' @param year The year of publication in string.
#' @param url The url of publication in string.
#' @param title The title of publication in string. 
#' 
#' @examples
#' getref("Amaliah, D", "2010", "https://datain360.com", "Testing")
#' 
#' @export
getref <- function(author, year, url, title){
  shiny::tags$li(class = "li-custom", glue::glue("{author}. ({year})."),
          shiny::tags$a(href = url,
                 title,
                 class = "externallink"
          )
  )
}


