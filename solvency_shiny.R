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
  actionButton(inputId = "cap.spouse", label = "Cap Spouse"),
  actionButton(inputId = "survivorjs75", label = "survivorjs75"),
  actionButton(inputId = "tax.max.90.FICA.134", label = "Tax Max to 90% and FICA to 134"),
  actionButton(inputId = "cola.chain.cpi", label = "COLA Chained CPI"),
  actionButton(inputId = "reduce.cola", label = "Reduce the COLA"),
  actionButton(inputId = "increase.fra", label = "Increase the Full Retirement Age"),
  actionButton(inputId = "increase.fra.era", label = "Increase FRA and ERA"),
  actionButton(inputId = "taxmax150000", label = "Increase the Tax Max to $150,000"),
  actionButton(inputId = "taxmax180000", label = "Increase the Tax Max to $180,000"),
  actionButton(inputId = "no.tax.max", label = "Eliminate the Tax Max"),

  plotOutput("hist")
  )

server <- function(input, output) {
  
  rv <- reactiveValues(data = solvency.m1)
  
  observeEvent(input$scheduled, {
    solvency <-  select(solvency, year, scheduled) 
    rv$data <- melt(solvency, id = 1)
  })
  
  observeEvent(input$mia, {
    solvency <-  select(solvency, year, scheduled, mini.pia)
    rv$data <- melt(solvency, id = 1)
  })
  
  observeEvent(input$tax.ssb, {
    solvency <-  select(solvency, year, scheduled, tax.ssb)
    rv$data <- melt(solvency, id = 1)
  })
  
  observeEvent(input$cap.spouse, {
    solvency <-  select(solvency, year, scheduled, cap.spouse)
    rv$data <- melt(solvency, id = 1)
  })
  
  observeEvent(input$survivorjs75, {
    solvency <-  select(solvency, year, scheduled, survivorjs75)
    rv$data <- melt(solvency, id = 1)
  })  

  observeEvent(input$tax.max.90.FICA.134, {
    solvency <-  select(solvency, year, scheduled, tax.max.90.FICA.134)
    rv$data <- melt(solvency, id = 1)
  })  
  
  observeEvent(input$cola.chain.cpi, {
    solvency <-  select(solvency, year, scheduled, cola.chain.cpi)
    rv$data <- melt(solvency, id = 1)
  })  
  
  observeEvent(input$reduce.cola, {
    solvency <-  select(solvency, year, scheduled, reduce.cola)
    rv$data <- melt(solvency, id = 1)
  })  

  observeEvent(input$increase.fra, {
    solvency <-  select(solvency, year, scheduled, increase.fra)
    rv$data <- melt(solvency, id = 1)
  }) 

  observeEvent(input$increase.fra.era , {
    solvency <-  select(solvency, year, scheduled, increase.fra.era )
    rv$data <- melt(solvency, id = 1)
  }) 
  
  observeEvent(input$taxmax150000, {
    solvency <-  select(solvency, year, scheduled, taxmax150000)
    rv$data <- melt(solvency, id = 1)
  }) 

  observeEvent(input$taxmax180000, {
    solvency <-  select(solvency, year, scheduled, taxmax180000)
    rv$data <- melt(solvency, id = 1)
  }) 
  
  observeEvent(input$no.tax.max, {
    solvency <-  select(solvency, year, scheduled, no.tax.max)
    rv$data <- melt(solvency, id = 1)
  }) 


  output$hist <- renderPlot({ 
   
    # Build graphic
    ggplot(rv$data, aes(x = year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Social Security Income/Benefit Ratio for Scheduled Benefits") + 
      ylim(-0.5, 1.5)
    
    })
}

shinyApp(ui = ui, server = server)