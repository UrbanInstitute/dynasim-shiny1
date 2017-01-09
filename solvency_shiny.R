## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)
library(scales)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
#source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R")
source("urban_institute_themes/urban_theme_windows.R")

# Source file for Mac
#source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R")
#source("urban_institute_themes/urban_theme_mac.R")

# Load data and gather data into long form for ggplot2
solvency <- read_csv("data/solvency.csv") %>%
    mutate(variable = factor(variable, unique(variable)))
cost.payroll <- read_csv("data/cost_payroll.csv") %>%
    mutate(variable = factor(variable, unique(variable)))
trust.fund.ratio <- read_csv("data/trust_fund_ratio.csv") %>%
    mutate(variable = factor(variable, unique(variable))) %>%
    mutate(value = value / 100)

ui <- fluidPage(
  
  theme = "shiny.css",
    
  titlePanel("Urban Institute Analysis of BPC Social Security Reforms"),
  
  fluidRow(
    column(4, 
      selectInput(inputId = "option", 
        label = "Social Security Reform", 
        choices = c("Scheduled Law and Payable Law" = "NULL",
                    "BPC Package" = "BPC Package",
                    "Annual PIA" = "Annual PIA", 
                    "Increase Benefits Taxation" = "Increase Benefits Taxation",
                    "Cap Spouse Benefits" = "Cap Spouse Benefits",
                    "75% Survivor Benefit" = "75% Survivor Benefit",
                    "90% Tax Max" = "90% Tax Max",
                    "90% Tax Max and 13.4% Payroll Tax" = "90% Tax Max and 13.4% Payroll Tax",
                    "Full Chained-CPI COLA" = "Full Chained-CPI COLA",
                    "Partial Chained-CPI COLA" = "Partial Chained-CPI COLA",
                    "Increase FRA" = "Increase FRA",
                    "Increase EEA & FRA" = "Increase FRA and EEA",
                    "$150,000 Tax Max" = "$150,000 Tax Max",
                    "$180,000 Tax Max" = "$180,000 Tax Max",
                    "Eliminate the Tax Max" = "Eliminate the Tax Max",
                    "13.4% Payroll Tax" = "13.4% Payroll Tax",
                    "14.4% Payroll Tax" = "14.4% Payroll Tax",
                    "15.4% Payroll Tax" = "15.4% Payroll Tax")),
      
      # Explanation of Social Security Reform
      htmlOutput("text1")),
      
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
   
    solvency %>%
      filter(variable == "Scheduled Law" | variable == "Payable Law" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
      scale_y_continuous(limits = c(-0.5, 1.5)) +
      labs(title = "Income to Benefits Ratio") + 
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
    
    cost.payroll %>%
      filter(variable == "Scheduled Law" | variable == "Payable Law" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
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

    trust.fund.ratio %>%
      filter(variable == "Scheduled Law" | variable == "Payable Law" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
      labs(title = "Trust Fund Ratio",
           caption = "DYNASIM4") +
      xlab("Calendar Year") +
      ylab(NULL) +
      scale_y_continuous(limits = c(-20, 5), labels = scales::percent) +
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
  
  # Explanation of Social Security Reform
  output$text1 <- renderText({
      
      if (input$option == "BPC Package") {"<br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to $0.03 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to beyond 2087."}
      else if (input$option == "Annual PIA") {"<br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> increases from -$10.59 trillion to -$14.19 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> decreases from 2034 to 2033."}
      else if (input$option == "Increase Benefits Taxation") {"Increases the taxation of 
          Social Security benefits <br/> <br/> 
          <strong>Open Group Unfunded Obligation</strong> increases from -$10.59 trillion to -$10.93 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> remains unchaged at 2034."}
      else if (input$option == "Cap Spouse Benefits") {"Caps the spouse benefit for 
          claimants who turn 60 in 2020 at $1,121.68 in 2016. Indexed the cap
          annually by chained CPI. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$9.94 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> remains unchanged at 2034."}
      else if (input$option == "75% Survivor Benefit") {"<br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$9.48 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> remains unchanged at 2034."}
      else if (input$option == "90% Tax Max") {"Raises the cap on annual earnings 
          subject to the Social Security payroll tax and that enter the benefits
          calculation to cover 90 percent of payroll. This increase is phased in
          over 10 years, beginning in 2016. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$6.97 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2042."}
      else if (input$option == "90% Tax Max and 13.4% Payroll Tax") {"Raises the cap on annual 
            earnings subject to the Social Security payroll tax and that enter the benefits
          calculation to cover 90 percent of payroll. This increase is phased in
          over 10 years, beginning in 2016. Also, increase the payroll tax to 
          13.4% over t10 years beginning in 2016. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$3.09 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2059."}
      else if (input$option == "Full Chained-CPI COLA") {"Ties beneficiaries' annual 
          cost-of-living-adjustment (COLA) to the change in the chained
          consumer price index (C-CPI-U), which grows more slowly than the 
          standard CPI-U now used to compute COLAs. (Only those NRA or older) <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.41 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2035."}
      else if (input$option == "Partial Chained-CPI COLA") {"Ties beneficiaries' annual 
          cost-of-living-adjustment (COLA) to the change in the chained
          consumer price index (C-CPI-U), which grows more slowly than the 
          standard CPI-U now used to compute COLAs. (All beneficiaries including
          those under the NRA) <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.72 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2035."}
      else if (input$option == "Increase FRA") {"Indefinitely raises Social 
          Security's FRA (now set at 67 beginning in 2022) and the age for 
          receiving delayed retirement credits by one month every two years, 
          beginning in 2024. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.69 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> remains unchanged at 2034."}
      else if (input$option == "Increase FRA and EEA") {"Raises Social Security's 
          early eligibility age (EEA), which is now set at 62, and indefinitely 
           raises Social Security's FRA (now set at 67 beginning in 2022) and 
          the age for receiving delayed retirement credits by one month every two years, 
          beginning in 2024. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.62 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> remains unchanged at 2034."}
      else if (input$option == "$150,000 Tax Max") {"Increase the tax cap to 
          $150,000 between 2016 and 2018 and then increase the tax cap by wage
          growth plus 0.5 percentage points thereafter. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$9.32 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2035."}
      else if (input$option == "$180,000 Tax Max") {"Increase the tax cap to 
          $180,000 between 2016 and 2018 and then increase the tax cap by wage
          growth plus 0.5 percentage points thereafter. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.76 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2036."}
      else if (input$option == "Eliminate the Tax Max") {"<br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$4.63 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2055."}
      else if (input$option == "13.4% Payroll Tax") {"Increase the payroll tax rate to 
          13.4% over 10 years beginning in 2016. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$7.05 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2039."}
      else if (input$option == "14.4% Payroll Tax") {"Increase the payroll tax rate to 
          14.4% over 10 years beginning in 2016. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$3.53 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2052."}
      else if (input$option == "15.4% Payroll Tax") {"Increase the payroll tax rate to 
          15.4% over 10 years beginning in 2016. <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$0.046 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> increases from 2034 to 2087."}
      else {"Current Law Scheduled and Current Law Payable <br/> <br/>
          <strong>Open Group Unfunded Obligation</strong> is -$10.59 trillion.<br/> <br/>
          <strong>Insolvency Year</strong> is 2034."}
      })
  
  # Chart 1
  output$hover_info1 <- renderUI({
    hover <- input$plot_hover1
    point <- nearPoints(solvency, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
        
    # calculate point position inside the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
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
    point <- nearPoints(cost.payroll, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
    point <- nearPoints(trust.fund.ratio, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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