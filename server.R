library(shiny)
library(data.table)

shinyServer(
  function(input, output) {
    if (!exists("pred.model")) {
      withProgress(message = 'Initializing', value = 0.1, {
        source("predict.R")
        incProgress(amount  <- 0.9, message  <- "Done.")
      })   
    }
    predictions  <- data.table()       
    # Return the word prediction and create the probabilty bar chart on button press only.
    output$prediction <- renderText({
      if (input$goButton == 0) ""
      else {
        isolate({
          words <- predictWord({input$sentence}, model=pred.model, maxPreds=4)
          output$others <- renderText({if (length(words) <= 1) "" else {
            paste0("Other possibilities were: ", paste(words[-1:0], collapse=", "))
          }})
          words[1]})}})})
