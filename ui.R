library(shiny)

shinyUI(fluidPage(
  column(
    width=10, offset=1,
    titlePanel("Text prediction shiny app"),
    fluidRow(
      # helpText("This data product predicts the next word in your sentence.\nPlease follow the steps."),
      # helpText("Enter your sentence."),
      textInput("sentence", "Enter your sentence:"),
      # helpText("Click to predict the next word."),
      actionButton("goButton", "Click to predict next word"),
      hr(),
      strong(p("Predicted Word:")),
      strong(textOutput("prediction")),
      hr(),
      textOutput("others")),
    title="Text prediction shiny app")))
