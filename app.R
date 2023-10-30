# Author: Angela Sofia Burkhart Colorado
# Purpose: This is a retirement plan calculator specifically for Spanish speaking audiences 

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

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
                                                   value = 3, 
                                                   min = 0, 
                                                   max = 100, 
                                                   step = 0.1),
                                      numericInput(inputId = "Y", 
                                                   label = "Years Till Retirement", 
                                                   value = 20), 
                                      numericInput(inputId = "AIR", 
                                                   label = "Annual Interest Rate (Percent)", 
                                                   value = 5, 
                                                   min = 0, 
                                                   max = 100, 
                                                   step = 0.01),
                                      actionButton(
                                        "submitbutton", 
                                        "Submit", 
                                        class="btn btn-primary")
                                    ), 
                             mainPanel(tags$label(h3('Results')), 
                                       tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                                       htmlOutput("summary"),
                                       plotOutput("barplot")
                                       # tags$img(
                                       #   src="logo.png", 
                                       #   width = "20%", 
                                       #   style="vertical-align:bottom"
                                       # )
                                       # imageOutput("logo"),
                                       # imageOutput("luci")

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
  # calculate bi-weekly contribution by employee 
  BiWeekEmployee <- reactive({
    round((input$AI*(input$C/100))/26, 2)
  })
  
  # calculate bi-weekly contribution by employer 
  BiWeekCompany <- reactive({
    if(input$C > 8){ 
      ec <- (input$AI*0.04)/26
    }else{
      ec <- round((input$AI*(input$C/100))/26, 2)/2
    }
      round(ec,2)
  })
  
  # calculate total contributed by employee 
  TotalEmployee <- reactive({
    round(input$AI*(input$C/100)*input$Y, 2)
  })
  
  # calculate balance upon retirement
  ## compound interest equation: BUR = PV(1+i)^n
  ## PV = present value (first deposit)
  ## i = rate of interest 
  ## n = number of periods (number of contributions)
  ## 26 contributions per year
  BalanceUponRetirement <- reactive({
    bwe <- input$AI*(input$C/100)
    bwc <- isolate(BiWeekCompany())*26
    PV <- round(bwe + bwc, 2)
    round(PV*((1+(input$AIR/100))^(1:input$Y)),2)
    
  })
  
  
  
  output$summary <- renderText({
    if(input$submitbutton > 0){
      str1 <- paste0("Bi-weekly contribution by employee: $", isolate(BiWeekEmployee()))
      str2 <- paste0("Bi-weekly contribution by company: $", isolate(BiWeekCompany()))
      str3 <- paste0("Total Contribution by employee after ", input$Y, " years: $", isolate(TotalEmployee()))
      str4 <- paste0("Balance Upon Retirement: $", sum(isolate(BalanceUponRetirement())))
      HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
    }
  })
  output$barplot <- renderPlot({
    if(input$submitbutton > 0){
      principal <- isolate(BalanceUponRetirement())
      interest <- principal*(input$AIR/100)
      df <- data.frame(Principal = principal, 
                       Interest = interest, 
                       Year = seq(1, input$Y, by = 1))
      df %>% 
        #mutate(Total = Principal + Interest) %>% 
        tidyr::gather(-Year, key = "category", value = "value") %>% 
        ggplot(aes(x = factor(Year), y = value, fill = category)) + 
          geom_bar(stat = "identity", position = "stack") + 
          theme_bw() + 
          theme(legend.title = element_blank(), 
                text = element_text(size = 12)) + 
          ylab("Balance") +
          xlab("Year")
    }
  })
}

shinyApp(ui, server)
