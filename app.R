## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(scales)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64c.exe")
#source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R")
source("urban_institute_themes/urban_theme_windows.R")

# Source file for Mac
#source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R")
#source("urban_institute_themes/urban_theme.R")

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

# Load data and gather data into long form for ggplot2
solvency <- read_csv("data/solvency.csv") %>%
    mutate(variable = factor(variable, unique(variable)))
cost.payroll <- read_csv("data/cost_payroll.csv") %>%
    mutate(variable = factor(variable, unique(variable)))
trust.fund.ratio <- read_csv("data/trust_fund_ratio.csv") %>%
    mutate(variable = factor(variable, unique(variable))) %>%
    mutate(value = value / 100)
summary <- read_csv("data/summary.csv")

option_text <- read_csv("text/option.csv")

ui <- fluidPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  
  theme = "shiny.css",
  
  fluidRow(
  
    column(8,
           
           titlePanel("Exploring Social Security Reform Options"),
           
           p("The Social Security trustees project that, by the mid-2030s, the system will no longer be able to pay all scheduled benefits. Which reform option should policymakers pursue to help balance the system?
             Use our interactive tool to compare how different groups would fare, over time, under the following policy options."),
           HTML("<p>Explore <b>the trust fund</b>, by income, by demographics, and <a href='http://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/dynasim-projecting-older-americans-future-well-being/detailed-projections-older-population-through-2065' target='_blank'>the data</a>.</p>"),
           
           br()
           )
  ),

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
      htmlOutput("text1"),
      htmlOutput("text2"),
      htmlOutput("text3"),
      htmlOutput("text4")),
      
      br(),

    column(4,
           style = "position:relative",
           
           h4("Income to Benefits Ratio"),
           plotOutput("plot1",
                      hover = hoverOpts("plot_hover1", delay = 100, delayType = "throttle")),
           uiOutput("hover_info1"))),
  
  fluidRow(
    column(4, 
           style = "position:relative",
           
           h4("Cost to Taxable Payroll Ratio"), 
           plotOutput("plot2",
                      hover = hoverOpts("plot_hover2", delay = 100, delayType = "debounce")),
           uiOutput("hover_info2")),
           
    column(4,
           style = "position:relative",
           
           h4("Trust Fund Ratio"),
           plotOutput("plot3",
                      hover = hoverOpts("plot_hover3", delay = 100, delayType = "debounce")),
           uiOutput("hover_info3"))),
  
  fluidRow(
    column(8,
           h3("Understand the Metrics"),
           HTML("<p><b>Income to Benefits Ratio:</b> The income to benefits ratio measures the adequacy of current OASDI trust fund income to cover current costs and benefits. It is total OASDI income from payroll taxes, taxation of benefits, general fund transfers, and interest divided by the total cost of scheduled OASDI benefits, administrative expenses, Railroad Retirement program benefits, and payments for vocational rehabilitation services for disabled beneficiaries.</p>"),  
           HTML("<p>When the ratio is one, the Social Security Administration spends one dollar for every dollar it collects or earns from interest. When the ratio is above one, the SSA brings in more money than it spends and the combined OASDI trust fund grows. When the ratio is below one, the SSA brings in less money than it spends and the combined OASDI trust fund shrinks.</p>"),
           HTML("<p><b>Annual Cost Rate:</b> The annual cost rate is a measure of the total cost of the OASDI programs compared to all of the taxable earnings in the economy. It is the cost of scheduled OASDI benefits, administrative expenses, Railroad Retirement program benefits, and payments for vocational rehabilitation services for disabled beneficiaries relative to taxable payroll for the year.  The ratio is projected to grow in the coming years because the baby-boom generation will increase the number of beneficiaries much faster than the number of workers increases.</p>"),
           HTML("<p><b>Trust Fund Ratio:</b> Trust fund ratios measure the percentage of a year’s costs that could be covered solely with money from the combined OASDI trust fund. They are the combined OASDI trust fund asset reserves at the beginning of a year expressed as a percentage of the total cost for the year.  A positive trust fund ratio means the combined OASDI trust fund was solvent in the prior year.</p>"),
           HTML("<p>Trust fund ratios are also important for assessing the long-term solvency of the combined OASDI trust fund. If the projected trust fund ratio is positive through the 75-year valuation period and is either level or increasing at the end of the period, then the trust fund is 'sustainably solvent.'</p>"),
           HTML("<p><b>Insolvency Year:</b> The insolvency year is the projected year when the combined OASDI trust fund will no longer be able to pay scheduled benefits in full on a timely basis.  The combined OASDI trust fund is currently expected to turn insolvent in 2034.</p>"),
           HTML("<p><b>Open Group Unfunded Obligation:</b> The open group unfunded obligation is a measure of the total shortfall (or surplus) of the OASDI trust fund over a valuation period in present value dollars.  It is present value non-interest income over the valuation period and starting trust fund asset reserves, minus the present value total costs of the OASDI program. The measure is in present value dollars because an additional dollar saved or earned in any given year has more time to accrue interest as special public-debt obligations in the combined OASDI trust funds than an additional dollar in a later year.</p>")   
    )
  )
)

server <- function(input, output) {

  output$plot1 <- renderPlot({ 
   
    solvency %>%
      filter(variable == "Scheduled Law" | variable == "Payable Law" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
        geom_hline(yintercept = 0) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(-0.5, 1.5)) +
        labs(title = NULL,
             x = "Calendar Year",
             y = NULL) + 
        theme(plot.margin = margin(t = -5),
              axis.line = element_blank())
    
    })
  
  output$plot2 <- renderPlot({ 
    
    cost.payroll %>%
      filter(variable == "Scheduled Law" | variable == "Payable Law" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, 0.31), expand = c(0, 0)) +
        labs(caption = "DYNASIM3
                        Urban Institute") + 
        xlab("Calendar Year") +
        ylab(NULL) +
        theme(plot.margin = margin(t = -5))
    
  })
  
  output$plot3 <- renderPlot({ 

    trust.fund.ratio %>%
      filter(variable == "Scheduled Law" | variable == "Payable Law" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
        geom_hline(yintercept = 0) +
        geom_line(size = 1) +
        labs(caption = "DYNASIM3
                        Urban Institute") +
        xlab("Calendar Year") +
        ylab(NULL) +
        scale_y_continuous(limits = c(-20, 5), labels = scales::percent) +
        theme(plot.margin = margin(t = -5),
              axis.line = element_blank())
    
  })
  
  # Explanation of Social Security Reform
  
  output$text1 <- renderText({
    
    as.character(
      option_text %>%
        filter(option == input$option) %>%
        select(text)
    )
  })
  
  output$text2 <- renderText({
    
    open.group.unfunded.liability <- summary %>%
      mutate(Option = ifelse(Option == "Current Law Scheduled", "NULL", Option)) %>%
      filter(Option == input$option) %>%
      select(`Open Group Unfunded Obligation`)
    
    as.character(
      option_text %>%
        filter(option == input$option) %>%
        mutate(text2 = paste(oguo1, as.character(open.group.unfunded.liability), oguo3)) %>%
        select(text2)
    )
  })
  
  output$text3 <- renderText({
    
    insolvency.year <- summary %>%
      mutate(Option = ifelse(Option == "Current Law Scheduled", "NULL", Option)) %>%
      filter(Option == input$option) %>%
      select(`Insolvency Year`)
    
    as.character(
      option_text %>%
        filter(option == input$option) %>%
        mutate(text3 = paste(insolvency1, as.character(insolvency.year), insolvency3)) %>%
        select(text3)
    )
  }) 
  
  output$text4 <- renderText({
    
    actuarial.balance <- summary %>%
      mutate(Option = ifelse(Option == "Current Law Scheduled", "NULL", Option)) %>%
      filter(Option == input$option) %>%
      select(`75-Year Actuarial Balance`)
  
    as.character(
      option_text %>%
        filter(option == input$option) %>%
        mutate(text4 = paste(actuarial.balance1, as.character(actuarial.balance), actuarial.balance3)) %>%
        select(text4)
    )
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