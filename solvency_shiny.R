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
    
  titlePanel("Urban Institute Analysis of BPC Social Security Reforms"),
  
  fluidRow(
    column(4, 
      selectInput(inputId = "option", 
        label = "Social Security Reform", 
        choices = c("BPC Package" = "bpc.package",
                    "Mini PIA" = "mini.pia", 
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
                    "15% FICA" = "fica15")),
      
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
   
    solvency.m %>%
      filter(variable == "scheduled" | variable == "payable" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
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

    trust.fund.ratio.m %>%
      filter(variable == "scheduled" | variable == "payable" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_line(size = 1) +
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
  
  # Explanation of Social Security Reform
  output$text1 <- renderText({
      
      if (input$option == "bpc.package") {"<br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> Beyond 2087"}
      else if (input$option == "mini.pia") {"<br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2033"}
      else if (input$option == "tax.ssb") {"Increases the taxation of 
          Social Security benefits <br/> <br/> 
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2033"}
      else if (input$option == "cap.spouse") {"Caps the spouse benefit for 
          claimants who turn 60 in 2020 at $1121.68 in 2016. Indexed the cap
          annually by chained CPI. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2034"}
      else if (input$option == "survivor.js75") {"<br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2034"}
      else if (input$option == "taxmax90") {"Raises the cap on annual earnings 
          subject to the Social Security payroll tax and that enter the benefits
          calculation to cover 90 percent of payroll. This increase is phased in
          over 10 years, beginning in 2016. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2041"}
      else if (input$option == "taxmax90.fica13.4") {"Raises the cap on annual 
            earnings subject to the Social Security payroll tax and that enter the benefits
          calculation to cover 90 percent of payroll. This increase is phased in
          over 10 years, beginning in 2016. Also, increase the payroll tax to 
          13.4% over t10 years beginning in 2016. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2060"}
      else if (input$option == "cola.chaincpi") {"Ties beneficiaries' annual 
          cost-of-living-adjustment (COLA) to the change in the chained
          consumer price index (C-CPI-U), which grows more slowly than the 
          standard CPI-U now used to compute COLAs. (Only those NRA or older) <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2035"}
      else if (input$option == "reduce.cola") {"Ties beneficiaries' annual 
          cost-of-living-adjustment (COLA) to the change in the chained
          consumer price index (C-CPI-U), which grows more slowly than the 
          standard CPI-U now used to compute COLAs. (All beneficiaries including
          those under the NRA) <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2035"}
      else if (input$option == "increase.fra") {"Indefinitely raises Social 
          Security's FRA (now set at 67 beginning in 2022) and the age for 
          receiving delayed retirement credits by one month every two years, 
          beginning in 2024. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2034"}
      else if (input$option == "increase.fra.era") {"Raises Social Security's 
          early eligibility age (EEA), which is now set at 62, and indefinitely 
           raises Social Security's FRA (now set at 67 beginning in 2022) and 
          the age for receiving delayed retirement credits by one month every two years, 
          beginning in 2024. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2034"}
      else if (input$option == "taxmax150000") {"Increase the tax cap to 
          $150,000 between 2016 and 2018 and then increase the tax cap by wage
          growth plus 0.5 percentage points thereafter. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2035"}
      else if (input$option == "taxmax180000") {"Increase the tax cap to 
          $180,000 between 2016 and 2018 and then increase the tax cap by wage
          growth plus 0.5 percentage points thereafter. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2036"}
      else if (input$option == "notaxmax") {"<br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2055"}
      else if (input$option == "fica13.4") {"Increase the payroll tax rate to 
          13.4% over 10 years beginning in 2016. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2039"}
      else if (input$option == "fica14") {"Increase the payroll tax rate to 
          14.4% over 10 years beginning in 2016. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> 2052"}
      else if (input$option == "fica15") {"Increase the payroll tax rate to 
          15.4% over 10 years beginning in 2016. <br/> <br/>
          <strong>Actuarial Deficit:</strong> <br/> <br/>
          <strong>Insolvency Year:</strong> Beyond 2087"}
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