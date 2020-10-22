#' Create A Reference List in Shiny App
#' 
#' This function aims to create a reference in a list format in the Shiny web app through an HTML tag. 
#' 
#' @param author The author/authors in string.
#' @param year The year of publication.
#' @param url The valid url of publication in string.
#' @param title The title of publication in string. 
#' 
#' @examples
#' getref("Amaliah, D", "2010", "https://datain360.com", "Testing")
#' 
#' @export
getref <- function(author, year, url, title){
  shiny::tags$li(class = "li-custom", paste0(author,". (", year, ")."),
          shiny::tags$a(href = url,
                 title,
                 class = "externallink"
          )
  )
}


