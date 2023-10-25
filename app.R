# Author: Angela Sofia Burkhart Colorado
# Purpose: This is a retirement plan calculator specifically for Spanish speaking audiences 

library(shiny)

ui <- fluidPage(
  numericInput(inputId = "AE", label = "Annual Earnings", value = 50000),
  numericInput(inputId = "PC", label = "Contribution", value = 0.2, min = 0, max = 1, step = 0.01),
  numericInput(inputId = "CC", label = "Company X Contributes", value = 0.1, min = 0, max = 0.4, step = 0.01),
  numericInput(inputId = "Y", label = "Years Till Retirement", value = 20), 
  verbatimTextOutput("value")
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)
