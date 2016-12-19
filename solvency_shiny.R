## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)
library(scales)

# Source file for Windows
#Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
#source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R")
#source("urban_institute_themes/urban_theme_windows.R")

# Source file for Mac
#source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R")
source("urban_institute_themes/urban_theme_mac.R")

# Load data and gather data into long form for ggplot2
solvency.m <- read_csv("data/solvency.csv") %>%
    gather(key = variable, value = value, -calendar.year) %>%
    mutate(variable = factor(variable, unique(variable)))
cost.payroll.m <- read_csv("data/cost_payroll.csv") %>%
    gather(key = variable, value = value, -calendar.year) %>%
    mutate(variable = factor(variable, unique(variable)))
trust.fund.ratio.m <- read_csv("data/trust_fund_ratio.csv") %>%
    gather(key = variable, value = value, -calendar.year) %>%
    mutate(variable = factor(variable, unique(variable))) %>%
    mutate(value = value / 100)

ui <- fluidPage(
  
  theme = "shiny.css",
    
  titlePanel("Urban Analysis of BPC Social Security Reforms"),
  
  fluidRow(
    column(4, 
      selectInput(inputId = "option", 
        label = "Social Security Reform", 
        choices = c("Mini PIA" = "mini.pia", 
                    "Tax SSB" = "tax.ssb",
                    "Cap Spouse" = "cap.spouse",
                    "SurvivorJS75" = "survivor.js75",
                    "90% Tax Max" = "taxmax90",
                    "90% Tax Max and 13.4 FICA" = "taxmax90.fica13.4",
                    "Chained-CPI COLA" = "cola.chaincpi",
                    "Reduce COLA" = "reduce.cola",
                    "Increase FRA" = "increase.fra",
                    "Increase ERA & FRA" = "increase.fra.era",
                    "Tax Max to $150,000" = "taxmax150000",
                    "Tax Max to $180,000" = "taxmax180000",
                    "Eliminate the Tax Max" = "notaxmax",
                    "13.4 FICA" = "fica13.4",
                    "14% FICA" = "fica14",
                    "15% FICA" = "fica15"))),
      br(),

    column(4,
           style = "position:relative",
           plotOutput("hist1",
                      hover = hoverOpts("plot_hover1", delay = 100, delayType = "debounce")),
           uiOutput("hover_info1"))),
  
  fluidRow(
    column(4, 
           style = "position:relative",
           plotOutput("hist2",
                      hover = hoverOpts("plot_hover2", delay = 100, delayType = "debounce")),
           uiOutput("hover_info2")),
           
    column(4,
           style = "position:relative",
           plotOutput("hist3",
                      hover = hoverOpts("plot_hover3", delay = 100, delayType = "debounce")),
           uiOutput("hover_info3")))
)

server <- function(input, output) {

  output$hist1 <- renderPlot({ 
   
    solvency.m %>%
      filter(variable == "scheduled" | variable == "payable" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1, position = position_dodge(width = 2)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title = "Income to Benefits Ratio") + 
      ylim(-0.5, 1.5) +
      xlab("Calendar Year") +
      ylab(NULL) +
      theme(axis.ticks.length = unit(0, "points"),
      axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                     unit = "pt",
                                                     valid.unit = 8L,
                                                     class = c("margin", "unit"))),
      axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                     unit = "pt",
                                                     valid.unit = 8L,
                                                     class = c("margin", "unit"))),
      legend.box.margin = margin(6, 0, 0, 0, "points"))
    
    })
  
  output$hist2 <- renderPlot({ 
    
    cost.payroll.m %>%
      filter(variable == "scheduled" | variable == "payable" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1, position = position_dodge(width = 2)) +
      scale_y_continuous(limits = c(0, 0.31), expand = c(0, 0)) +
      labs(title = "Cost to Taxable Payroll Ratio",
           caption = " ") + 
      xlab("Calendar Year") +
      ylab(NULL) +
      theme(axis.ticks.length = unit(0, "points"),
            axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                          unit = "pt",
                                                          valid.unit = 8L,
                                                          class = c("margin", "unit"))),
            axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                          unit = "pt",
                                                          valid.unit = 8L,
                                                          class = c("margin", "unit"))),
            legend.box.margin = margin(6, 0, 0, 0, "points"))
    
  })
  
  output$hist3 <- renderPlot({ 

    trust.fund.ratio.m %>%
      filter(variable == "scheduled" | variable == "payable" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1, position = position_dodge(width = 2)) +
      labs(title = "Trust Fund Ratio",
           caption = "DYNASIM4") +
      ylim(-2000, 500) +
      xlab("Calendar Year") +
      ylab(NULL) +
      scale_y_continuous(expand = c(0.3, 0), labels = scales::percent) +
      theme(axis.ticks.length = unit(0, "points"),
      axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                      unit = "pt",
                                                      valid.unit = 8L,
                                                      class = c("margin", "unit"))),
      axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                    unit = "pt",
                                                    valid.unit = 8L,
                                                    class = c("margin", "unit"))),
      legend.box.margin = margin(6, 0, 0, 0, "points"))
    
  })
  
  # Chart 1
  output$hover_info1 <- renderUI({
    hover <- input$plot_hover1
    point <- nearPoints(solvency.m, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
        
    # calculate point position inside the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    #TODO(awunderground): change CSS colors of pop-up
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Year:  </b>", point$calendar.year,"<br/>",
                    "<b> Ratio: </b>", round(point$value, 2), "<br/>"
                    )))
    )
  })
  
  # Chart 2
  output$hover_info2 <- renderUI({
    hover <- input$plot_hover2
    point <- nearPoints(cost.payroll.m, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    print(point)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Year:  </b>", point$calendar.year,"<br/>",
                    "<b> Ratio: </b>", round(point$value, 2), "<br/>"
                    )))
    )
  })
  
  # Chart 3
  output$hover_info3 <- renderUI({
    hover <- input$plot_hover3
    point <- nearPoints(trust.fund.ratio.m, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    print(point)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Year:  </b>", point$calendar.year,"<br/>",
                    "<b> Ratio: </b>", round(point$value, 2), "<br/>"
                    )))
    )
  })
}

shinyApp(ui = ui, server = server)