# ui.R

library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Placeholder"),
  sidebarPanel(h2("yada")),
  mainPanel(textOutput("main"))
))
