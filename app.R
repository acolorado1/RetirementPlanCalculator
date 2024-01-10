library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(markdown)

# Define UI for application 
ui <- bootstrapPage(
  theme = shinytheme("united"),
  navbarPage("Estimar sus ahorros", 
             tags$head(tags$style(HTML('.navbar-static-top {background-color: #007078;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: #007078;}',
                                       '.navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {
                                            color: #ffffff;
                                            background-color: #FFB600;}', 
                                       '.navbar-default {
                                            border-color: #FFB600;}', 
                                       '#submitbutton{
                                            background-color: #007078;
                                            border-color: #007078;'))),
             tabPanel("Inicio",
                      div(class = "row", 
                          div(class = "col-lg-12", 
                              HTML("<center><h4>¿Cuánto dinero PODRÍA tener cuando se retire? Esta calculadora le mostrará una cantidad aproximada. Esto NO es una garantía, los resultados pueden variar.</h4></center>"),
                              div(class = "well", 
                                  HTML("<h3>Introduzca su información</h3>"),
                                  numericInput(inputId = "AI", 
                                               label = "Ingreso anual APROXIMADO", 
                                               value = 50000, 
                                               min = 1, 
                                               step = 1),
                                  numericInput(inputId = "C", 
                                               label = "Porcentaje de sus ingresos, aportado en cada cheque de sueldo (Sonnenalp empieza aportando un 3% automáticamente).", 
                                               value = 3, 
                                               min = 0, 
                                               max = 100, 
                                               step = 0.1),
                                  numericInput(inputId = "Y", 
                                               label = "¿Cuántos años trabajará hasta que se retire?", 
                                               value = 20, 
                                               min = 1, 
                                               step = 1), 
                                  numericInput(inputId = "SI", 
                                               label = "Aumento aproximado anual (el promedio es un 3%)", 
                                               value = 3,
                                               min = 0, 
                                               max = 100, 
                                               step = 0.01),
                                  numericInput(inputId = "AIR", 
                                               label = "Tasa de interés aproximado que obtendrá (el promedio es el 5%)", 
                                               value = 5, 
                                               min = 0, 
                                               max = 100, 
                                               step = 0.01),
                                  actionButton(
                                    "submitbutton", 
                                    "Submit", 
                                    class="btn btn-primary")
                              )# end div class well
                          ), 
                          div(class = "col-lg-12",
                              tags$label(h3('¡SU AHORRO PARA EL RETIRO!')), 
                              htmlOutput("summarye"),
                              htmlOutput("summaryc"),
                              htmlOutput("bigoutput"),
                              plotOutput("barplot"),
                              tags$head(tags$style("#summarye{
                                                    color: #FFB600;
                                                    font-style: bold;
                                       }")),
                              tags$head(tags$style("#summaryc{
                                                    color: #75abc9;
                                                    font-style: bold;
                                       }")),
                              tags$head(tags$style("#bigoutput{
                                                    color: #007078;
                                                    font-size: 20px;
                                                    font-style: bold;
                                       }"))
                          ), # results page
                          div(class = "col-lg-12", 
                              imageOutput("graphic"),
                              tags$head(tags$style("#graphic{
                                                    text-align: center;
                                       }"))
                          ) # end image row 
                      )
             )#end of "home" tab
  )#end of navbar page 
)

server <- function(input, output) {
  # get yearly salary vector 
  salary <- eventReactive(input$"submitbutton", {
    if(input$SI > 0){
      cumprod(c(input$AI, rep(1+input$SI/100, input$Y-1)))
    }else{
      rep(input$AI, input$Y)
    }
  })
  
  # yearly employee contribution (ec)
  ec <- eventReactive(input$"submitbutton", {
    round(salary()*(input$C/100),2)
  })
  
  # yearly company contribution (cc)
  cc <- eventReactive(input$"submitbutton", {
    if(input$C > 8){ 
      round(salary()*0.04,2)
    }else{
      round(ec()/2, 2)
    }
  })
  
  # calculate balance upon retirement
  ## compound interest equation: BUR = PV(1+i)^n
  ## PV = present value (first deposit)
  ## i = rate of interest 
  ## n = number of periods (number of contributions)
  ## 26 contributions per year
  balance <- eventReactive(input$"submitbutton", {
    total.pv <- ec() + cc()
    round(total.pv*((1+(input$AIR/100))^(1:input$Y)),2)
  })
  

  # create summary employee outputs 
  output$summarye <- renderText({
    if(input$submitbutton > 0){
      str1 <- paste0("En ", input$Y, " años, sus aportes por cheque de sueldo pasarán de $", round(ec()[1]/26,2), " a $", round(ec()[input$Y]/26,2))
      str2 <- paste0("Su aporte total después de ", input$Y, " años es: $", formatC(sum(ec()), format = "f", digits=2, big.mark=","))
      HTML(paste(str1, str2, sep = "<br/>"))
    }
  })
  
  # create summary company outputs 
  output$summaryc <- renderText({
    if(input$submitbutton > 0){
      str1 <- paste0("En ", input$Y, " años, los aportes de la empresa por cheque de sueldo pasarán de $", round(cc()[1]/26,2), " a $", round(cc()[input$Y]/26,2))
      str2 <- paste0("El aporte total de la empresa después de ", input$Y, " años es: $", formatC(sum(cc()), format = "f", digits=2, big.mark=","))
      HTML(paste(str1, str2, sep = "<br/>"))
    }
  })
  
  # main output is balance upon retirement 
  output$bigoutput <- renderText({
    if(input$submitbutton > 0){
      str1 <- paste0("SU AHORRO APROXIMADO PARA EL RETIRO: $", formatC(sum(balance()), format = "f", digits=2, big.mark=","))
      HTML(paste(str1, sep = "<br/>"))
    }
  })
  
  #generate barplot with increasing balance
  output$barplot <- renderPlot({
    if(input$submitbutton > 0){
      
      # cumulative sum over balance of account, ec, and cc
      total <- cumsum(balance())
      ec.t <- cumsum(ec())
      cc.t <- cumsum(cc())
      
      # create df
      year <- seq(1, input$Y, by = 1)
      df <- data.frame(Year = year,
                       total = total,
                       EmployeeContribution = ec.t,
                       CompanyContribution = cc.t)
      
      # make sure there is no scientific notation 
      options(scipen=5)
      
      # create plot
      df %>%
        mutate(Interest = total - EmployeeContribution - CompanyContribution) %>%
        select(-total) %>%
        gather(-Year, key = "category", value = "value") %>%
        mutate(category = case_when(category == "Interest" ~ "Intereses",
                                    category == "EmployeeContribution" ~ "Su aporte",
                                    category == "CompanyContribution" ~ "Aporte de la empresa"),
               category = factor(category, levels = c("Intereses", "Aporte de la empresa", "Su aporte")),
               Year = factor(Year)) %>%
        ggplot(aes(x = Year, y = value, fill = category)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("Aporte de la empresa" = "#AFE2FE", 
                                     "Su aporte" = "#FFB600", 
                                     "Intereses" = "#007078")) +
        theme_bw() +
        theme(legend.title = element_blank(),
              legend.position = c(0.35, 0.85), 
              text = element_text(size = 16),
              legend.box.background = element_rect(colour = "black")) +
        scale_x_discrete(breaks = as.integer(seq(from = 1, to = input$Y, length = 10))) + 
        ylab("SU AHORRO ($)") +
        xlab("AÑOS DE INVERSIÓN")
      
    }
  })
  output$graphic <- renderImage({
    list(src = "www/graphic.png",
         contentType = "image/png",
         width = "30%",
         height = "auto"
    )
    
  }, deleteFile = F)
}

# Run the application 
shinyApp(ui = ui, server = server)
