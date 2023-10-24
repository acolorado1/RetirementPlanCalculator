# Author: Angela Sofia Burkhart Colorado
# Purpose: This is a retirement plan calculator specifically for Spanish speaking audiences 

library(shiny)

ui <- fluidPage(
  selectInput("Annual Earnings", label = "dollars"),
  selectInput("Contribution", label = "percent"),
  selectInput("Company X Contributes", label = "percent"),
  selectInput("Years Till Retirement", label = "years") 
              
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)
