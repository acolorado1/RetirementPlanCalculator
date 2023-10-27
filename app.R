# Author: Angela Sofia Burkhart Colorado
# Purpose: This is a retirement plan calculator specifically for Spanish speaking audiences 

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("united"), 
                navbarPage("Retirement Plan Calculator", 
                           tabPanel("Home", 
                                    sidebarPanel(
                                      HTML("<h3>Input Your Information</h3>"),
                                      numericInput(inputId = "AI", 
                                                   label = "Annual Income", 
                                                   value = 50000),
                                      numericInput(inputId = "C", 
                                                   label = "Employee Bi-Weekly Contribution (Percent of Salary)", 
                                                   value = 0.03, 
                                                   min = 0, 
                                                   max = 1, 
                                                   step = 0.01),
                                      numericInput(inputId = "Y", 
                                                   label = "Years Till Retirement", 
                                                   value = 20), 
                                      numericInput(inputId = "AIR", 
                                                   label = "Annual Interest Rate", 
                                                   value = 0.05, 
                                                   min = 0, 
                                                   max = 1, 
                                                   step = 0.001),
                                      actionButton(
                                        "submitbutton", 
                                        "Submit", 
                                        class="btn btn-primary")
                                    ), 
                             mainPanel(tags$label(h3('Results')), 
                                       tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                                       verbatimTextOutput("summary"),
                             )
                         ), #tab panel: home 
               tabPanel("About", 
                        titlePanel("About"), 
                        div(includeMarkdown("about.md"),
                            align="justify")
               ) #table panel: about
  )#navbar page 
  
)# fluid page 

server <- function(input, output, session){
  numbersInput <- reactive({
    # Assumption: 52 weeks in a year -> 26 contributions per year 
    
    # calculate bi-weekly contribution (bwc)
    ## (Annual Income * Percent of Salary)/26  
    bwc <- round((input$AI*input$C)/26, 2)
    
    # calculate contribution by employer (ec) 
    ## default is 50% of employee contribution with a max of 5% of salary 
    if(input$C > 0.08){ 
      ec <- (input$AI*0.04)/26
    }else{
      ec <- bwc/2
    }
    ec <- round(ec,2)
    
    # number of contributions (nc)
    ## years till retirement * 26 
    nc <- input$Y*26
    
    # calculate balance upon retirement (BUR)
    ## compound interest equation: BUR = PV(1+i)^n
    ## PV = present value (first deposit)
    ## i = rate of interest 
    ## n = number of periods (number of contributions - 1)
    PV <- round(bwc + ec, 2)
    principal <- round(PV*((1+input$AIR)^(0:nc-1)),2)
    interest <- principal*input$AIR
    BUR <- sum(interest)
    
    # calculate total contribution by employee (tce)
    tce <- (input$AI*input$C)*input$Y
    
    print(paste0("bi-weekly contributions: ", bwc))
    print(paste0("bi-weekly employer contribution: ", ec))
    print(paste0("number of contributions: ", nc))
    print(paste0("installment value: ", PV))
    
    
    #output text summary: balance upon retirement, total contribution by employee
    summary_statement <- paste0("Over the course of ",
                                input$Y,
                                " years, the employee will have contributed $",
                                tce, " and their balance upon retirement will be $", BUR)
    print(summary_statement)
  })
  
  output$summary <- renderText({
    if(input$submitbutton > 0){
      isolate(numbersInput())
      
    }
  })
}

shinyApp(ui, server)
