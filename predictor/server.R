# server.R

library(shiny)
# library(rJava)
# library(openNLP)

shinyServer(function(input, output) {
  
  output$main <- renderText({
    99
  })
  
})