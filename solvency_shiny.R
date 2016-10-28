library(shiny)
library(ggplot2)
library(reshape2)

solvency <- read.csv("solvency.csv", header = TRUE)







ui <- fluidPage(
  
  actionButton(inputId = "scheduled", label = "Scheduled"),
  actionButton(inputId = "mia", label = "Mia"),
  actionButton(inputId = "tax.ssb", label = "Tax SSB"),
  
  plotOutput("hist")
  )

server <- function(input, output) {
  
  data <- eventReactive(input$scheduled, {
    solvency <-  select(solvency, year, scheduled) 
    solvency.m <- melt(solvency, id = 1)
  })
  
  data <- eventReactive(input$mia, {
    solvency <-  select(solvency, year, mini.pia)
    solvency.m <- melt(solvency, id = 1)
  })
  
  data <- eventReactive(input$mia, {
    solvency <-  select(solvency, year, tax.ssb)
    solvency.m <- melt(solvency, id = 1)
  })
  
  output$hist <- renderPlot({ 
   
    # Build graphic
    ggplot(data(), aes(x = year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Social Security Income/Benefit Ratio for Scheduled Benefits")
    
    })
}

shinyApp(ui = ui, server = server)