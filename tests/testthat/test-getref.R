library(shiny)
test_that("getref return html", {
  usage <- getref("Amaliah, D", 
                  "2020", 
                  "https://datain360.com",
                  "Testing")
  expected <- tags$li(class = "li-custom", "Amaliah, D. (2020).",
                      tags$a(href = "https://datain360.com",
                             "Testing",
                             class = "externallink"))
  
  expect_equivalent(usage, expected)
})
