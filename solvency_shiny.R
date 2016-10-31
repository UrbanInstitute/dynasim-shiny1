## Libraries and Source Files
library(shiny)
library(tidyverse)
library(reshape2)
library(extrafont)
library(grid)
library(RColorBrewer)

Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Urban theme for Windows
source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')

# Load data
solvency <- read.csv("data\\solvency.csv", header = TRUE, stringsAsFactors = FALSE)
cost.payroll <- read.csv("data\\cost_payroll.csv", header = TRUE, stringsAsFactors = FALSE)
trust.fund.ratio <- read.csv("data\\trust_fund_ratio.csv", header = TRUE, stringsAsFactors = FALSE)

solvency.m <- tbl_df(melt(solvency, id = 1))
cost.payroll.m <- tbl_df(melt(cost.payroll, id = 1))
trust.fund.ratio.m <- tbl_df(melt(trust.fund.ratio, id = 1))

ui <- fluidPage(
  
  titlePanel("Urban Analysis of BPC Social Security Reforms"),

  sidebarLayout(
  sidebarPanel(selectInput(inputId = "option", 
              label = "Social Security Reform", 
              choices = c("Scheduled" = "scheduled", 
                          "Payable" = "payable",
                          "Mini.PIA" = "mini.pia", 
                          "Tax SSB" = "tax.ssb",
                          "Cap Spouse" = "cap.spouse",
                          "SurvivorJS75" = "survivor.js75",
                          "90% Tax max" = "taxmax90",
                          "90% Tax max and 13.4 FICA" = "taxmax90.fica13.4",
                          "13.4 FICA" = "fica13.4",
                          "Chained-CPI COLA" = "cola.chaincpi",
                          "Reduce COLA" = "reduce.cola",
                          "Increase FRA" = "increase.fra",
                          "Increase ERA & FRA" = "increase.fra.era",
                          "Tax Max to $150,000" = "taxmax150000",
                          "Tax Max to $180,000" = "taxmax180000",
                          "Eliminate the Tax Max" = "notaxmax",
                          "14% FICA" = "fica14",
                          "15% FICA" = "fica15"))),

  mainPanel(plotOutput("hist1"),
            plotOutput("hist2"),
            plotOutput("hist3"))
  ))

server <- function(input, output) {

  output$hist1 <- renderPlot({ 
   
    # Build graphic

    solvency.m %>%
      filter(variable == "scheduled" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Social Security Income/Benefit Ratio") + 
      ylim(-0.5, 1.5)
    
    })
  
  output$hist2 <- renderPlot({ 
    
    # Build graphic
    
    cost.payroll.m %>%
      filter(variable == "scheduled" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Social Security Cost/Payroll Ratio") + 
      ylim(0, 0.5)
    
  })
  
  output$hist3 <- renderPlot({ 
    
    # Build graphic
    
    trust.fund.ratio.m %>%
      filter(variable == "scheduled" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Social Security Trust Fund Ratio") +
      ylim(-1500, 500)
    
  })
}

shinyApp(ui = ui, server = server)