# Author: Angela Sofia Burkhart Colorado
# Purpose: This is a retirement plan calculator specifically for Spanish speaking audiences 

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

ui <- fluidPage(theme = shinytheme("united"), 
                navbarPage("Retirement Plan Calculator", 
                           tabPanel("Home", 
                                    fixedRow(
                                      column(4, offset = 0, 
                                             sidebarPanel( width = "95%",  
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
                                             ), # end side bar
                                             imageOutput("logo")
                                      ), # end column 1
                                      column(6, offset = 0,
                                             mainPanel(width = "100%",
                                                       tags$label(h3('Results')), 
                                                       htmlOutput("summary"),
                                                       plotOutput("barplot")
                                             ) # end main panel 
                                      ), # end column 2 
                                      column(2, offset = 0,
                                             imageOutput("luci")
                                      ) # end column 3 
                                    ), # end first row 
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
      present.value <- isolate(BalanceUponRetirement())
      total <- c()
      for (i in 1:length(present.value)) {
        s <- sum(present.value[1:i])
        total <- c(total, c(s))
      }
      interest <- total*(input$AIR/100)
      df <- data.frame(Year = seq(1, input$Y, by = 1),
                       total = total, 
                       Interest = interest, 
                       EmployeeContribution = rep(input$AI*(input$C/100), input$Y), 
                       CompanyContribution = rep(isolate(BiWeekCompany())*26,input$Y))
      df %>% 
        mutate(Principal = total - Interest - EmployeeContribution - CompanyContribution) %>% 
        select(-total) %>% 
        tidyr::gather(-Year, key = "category", value = "value") %>% 
        mutate(category = factor(category, levels = c("Principal", "Interest", "EmployeeContribution", "CompanyContribution"))) %>% 
        ggplot(aes(x = factor(Year), y = value, fill = category)) + 
          geom_bar(stat = "identity", position = "stack") + 
          scale_fill_manual(values = c("Principal" = "#00BFCA", "Interest" = "#F8766D", 
                              "EmployeeContribution" = "#000080", "CompanyContribution" = "#ffd700")) +
          theme_bw() + 
          theme(legend.title = element_blank(), 
                text = element_text(size = 16)) + 
          ylab("Balance ($)") +
          xlab("Year")
    }
  })
  
  
  output$luci <- renderImage({
    list(src = "www/Luci2.png",
         contentType = "image/png",
         width = "100%",
         height = "auto",
         align = "right"
    )

  }, deleteFile = F)
  
  output$logo <- renderImage({
    list(src = "www/logo.png",
         contentType = "image/png",
         width = "50%",
         height = "auto"
    )

  }, deleteFile = F)
}

shinyApp(ui, server)
