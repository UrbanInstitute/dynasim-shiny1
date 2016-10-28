library(shiny)
library(tidyverse)
library(reshape2)
library(extrafont)
library(grid)
library(RColorBrewer)

#ys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Source file for Windows
source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')

solvency <- read.csv("solvency.csv", header = TRUE, stringsAsFactors = FALSE)

solvency.m <- tbl_df(melt(solvency, id = 1))

solvency.m <- mutate(solvency.m, variable = as.character(variable))

ui <- fluidPage(
  
  titlePanel("Urban Analysis of BPC Social Security Reforms"),

  selectInput(inputId = "option", 
              label = "Social Security Reform", 
              choices = c("Scheduled" = "scheduled", 
                          "Mini.PIA" = "mini.pia", 
                          "Tax SSB" = "tax.ssb",
                          "Cap Spouse" = "cap.spouse",
                          "SurvivorJS75" = "survivorjs75",
                          "90% Tax max and 13.4 FICA" = "tax.max.90.FICA.134",
                          "Chained-CPI COLA" = "cola.chain.cpi",
                          "Reduce COLA" = "reduce.cola",
                          "Increase FRA" = "increase.fra",
                          "Increase ERA & FRA" = "increase.fra.era",
                          "Tax Max to $150,000" = "taxmax150000",
                          "Tax Max to $180,000" = "taxmax180000",
                          "Eliminate the Tax Max" = "no.tax.max"
                          )),

  
  plotOutput("hist")
  )

server <- function(input, output) {

  output$hist <- renderPlot({ 
   
    # Build graphic

    solvency.m %>%
      filter(variable == "scheduled" | variable == input$option) %>%
      ggplot(aes(x = year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Social Security Income/Benefit Ratio for Scheduled Benefits") + 
      ylim(-0.5, 1.5)
    
    })
}

shinyApp(ui = ui, server = server)