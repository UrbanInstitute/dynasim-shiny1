## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)

# Source file for Windows
#Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')
#source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R')
source('urban_institute_themes/urban_theme_mac.R')

# Load data and gather data into long form for ggplot2
solvency.m <- read_csv("data/solvency.csv") %>%
    gather(key = variable, value = value, -calendar.year)
cost.payroll.m <- read_csv("data/cost_payroll.csv") %>%
    gather(key = variable, value = value, -calendar.year)
trust.fund.ratio.m <- read_csv("data/trust_fund_ratio.csv") %>%
    gather(key = variable, value = value, -calendar.year)

ui <- fluidPage(
  
  titlePanel("Urban Analysis of BPC Social Security Reforms"),
  
  fluidRow(
    column(4, 
      selectInput(inputId = "option", 
        label = "Social Security Reform", 
        choices = c("Mini.PIA" = "mini.pia", 
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
      br(),

    column(4,
      plotOutput("hist1"))),
  
  fluidRow(
    column(4, 
      plotOutput("hist2")),
           
    column(4,
      plotOutput("hist3")))
  )

server <- function(input, output) {

  output$hist1 <- renderPlot({ 
   
    # Build graphic

    solvency.m %>%
      filter(variable == "scheduled" | variable == "payable" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Income:Benefits Ratio") + 
      ylim(-0.5, 1.5) +
      xlab("Calendar Year") +
      ylab("Income:Benfits Ratio")
    
    })
  
  output$hist2 <- renderPlot({ 
    
    # Build graphic
    
    cost.payroll.m %>%
      filter(variable == "scheduled" | variable == "payable" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Cost:Taxable Payroll Ratio") + 
      ylim(0, 0.3) +
      xlab("Calendar Year") +
      ylab("Cost:Taxable Payroll Ratio")
    
  })
  
  output$hist3 <- renderPlot({ 
    
    # Build graphic
    
    trust.fund.ratio.m %>%
      filter(variable == "scheduled" | variable == "payable" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      ggtitle("Trust Fund Ratio") +
      ylim(-2000, 500) +
      xlab("Calendar Year") +
      ylab("Trust Fund Ratio")
    
  })
}

shinyApp(ui = ui, server = server)