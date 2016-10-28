library(shiny)
library(tidyverse)
library(reshape2)
library(extrafont)
library(grid)
library(RColorBrewer)

Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Source file for Windows
source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')

solvency <- read.csv("solvency.csv", header = TRUE)

solvency1 <- select(solvency, year, scheduled) 
solvency.m1 <- melt(solvency1, id = 1)



ui <- fluidPage(
  
  actionButton(inputId = "scheduled", label = "Scheduled"),
  actionButton(inputId = "mia", label = "Mia"),
  actionButton(inputId = "tax.ssb", label = "Tax SSB"),
  
  plotOutput("hist")
  )

server <- function(input, output) {
  
  rv <- reactiveValues(data = solvency.m1)
  
  observeEvent(input$scheduled, {
    solvency <-  select(solvency, year, scheduled) 
    rv$data <- melt(solvency, id = 1)
  })
  
  observeEvent(input$mia, {
    solvency <-  select(solvency, year, mini.pia)
    rv$data <- melt(solvency, id = 1)
  })
  
  observeEvent(input$tax.ssb, {
    solvency <-  select(solvency, year, tax.ssb)
    rv$data <- melt(solvency, id = 1)
  })
  
  output$hist <- renderPlot({ 
   
    # Build graphic
    ggplot(rv$data, aes(x = year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Social Security Income/Benefit Ratio for Scheduled Benefits")
    
    })
}

shinyApp(ui = ui, server = server)