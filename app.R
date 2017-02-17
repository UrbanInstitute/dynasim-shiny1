## Libraries and Source Files
library(shiny)
library(tidyverse)
library(extrafont)
library(grid)
library(RColorBrewer)
library(scales)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64c.exe")
#source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R")
source("urban_institute_themes/urban_theme_windows.R")

# Source file for Mac
#source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R")
#source("urban_institute_themes/urban_theme_mac.R")

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"


# Load data and gather data into long form for ggplot2
solvency <- read_csv("data/solvency.csv") %>%
    mutate(variable = factor(variable, unique(variable)))
cost.payroll <- read_csv("data/cost_payroll.csv") %>%
    mutate(variable = factor(variable, unique(variable)))
trust.fund.ratio <- read_csv("data/trust_fund_ratio.csv") %>%
    mutate(variable = factor(variable, unique(variable))) %>%
    mutate(value = value / 100)

ui <- fluidPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  
  theme = "styles.css",
  
  fluidRow(
  
    column(8,
           
           titlePanel("Exploring Social Security Reform Options"),
           
           p("The Social Security trustees project that, by the mid-2030s, the system will no longer be able to pay all scheduled benefits. Which reform option should policymakers pursue to help balance the system?
             Use our interactive tool to compare how different groups would fare, over time, under the following policy options."),
           HTML("<p>Explore <b>the trust fund</b>, by income, by demographics, and <a href='http://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/dynasim-projecting-older-americans-future-well-being/detailed-projections-older-population-through-2065'>the data</a>.</p>"),
           
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

  output$hist1 <- renderPlot({ 
   
    solvency %>%
      filter(variable == "Scheduled Law" | variable == "Payable Law" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_hline(yintercept = 0) +
      geom_line(size = 1) +
      scale_y_continuous(limits = c(-0.5, 1.5)) +
      labs(title = "Income to Benefits Ratio") + 
      xlab("Calendar Year") +
      ylab(NULL) +
      theme(text = element_text(family = "Lato"),
            axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                     unit = "pt",
                                                     valid.unit = 8L,
                                                     class = c("margin", "unit"))),
            axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                     unit = "pt",
                                                     valid.unit = 8L,
                                                     class = c("margin", "unit"))),
      legend.box.margin = margin(6, 0, 0, 0, "points"),
      plot.title = element_text(size = 18, hjust = -0.15),
      axis.line = element_line(colour = "white"))
    
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
      theme(axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                          unit = "pt",
                                                          valid.unit = 8L,
                                                          class = c("margin", "unit"))),
            axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                          unit = "pt",
                                                          valid.unit = 8L,
                                                          class = c("margin", "unit"))),
            legend.box.margin = margin(6, 0, 0, 0, "points"),
            plot.title = element_text(size = 18, hjust = -0.2))
    
  })
  
  output$hist3 <- renderPlot({ 

    trust.fund.ratio %>%
      filter(variable == "Scheduled Law" | variable == "Payable Law" | variable == input$option) %>%
      ggplot(aes(x = calendar.year, y = value, colour = variable)) +
      geom_hline(yintercept = 0) +
      geom_line(size = 1) +
      labs(title = "Trust Fund Ratio",
           caption = "DYNASIM4") +
      xlab("Calendar Year") +
      ylab(NULL) +
      scale_y_continuous(limits = c(-20, 5), labels = scales::percent) +
      theme(axis.text.x = element_text(margin = structure(c(4, 0, 0, 0),
                                                      unit = "pt",
                                                      valid.unit = 8L,
                                                      class = c("margin", "unit"))),
      axis.text.y = element_text(margin = structure(c(0, 2, 0, 0),
                                                    unit = "pt",
                                                    valid.unit = 8L,
                                                    class = c("margin", "unit"))),
      legend.box.margin = margin(6, 0, 0, 0, "points"),
      plot.title = element_text(size = 18, hjust = -0.26),
      axis.line = element_line(colour = "white"))
    
  })
  
  # Explanation of Social Security Reform
  output$text1 <- renderText({
      
      if (input$option == "BPC Package") {"<p>Annual PIA, limit spousal benefits, replace the WEP and GPO with a proportional reduction in OASI benefits based on covered earnings, enhance survivor benefits, increase the progressivity of the benefit formula, increase Social Security tax max to $195,000, payroll tax to 13.4% and FRA to 69, switch to C-CPI-U for COLAs, end 'claim-and-suspend' games, create a basic minimum benefit for all individuals above the FRA eligible for Social Security, and tax 100 percent of Social Security benefits for beneficiaries with annual incomes above $250,000.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to $0.03 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to beyond 2087.</p>"}
      
      else if (input$option == "Annual PIA") {"<p>Eliminates the preferential treatment of workers with short careers by applying Social Security’s progressive benefit formula to the 40 highest years of wage-indexed earnings divided by 37 rather than applying the formula to total wage-indexed earnings received in the top 35 years. It also makes the benefit formula more progressive. This begins with OASI claimants who attain age 62 in 2022.</p>
          <p><strong>Open Group Unfunded Obligation</strong> increases from -$10.59 trillion to -$14.19 trillion.</p>
          <p><strong>Insolvency Year</strong> decreases from 2034 to 2033.</p>"}
      
      else if (input$option == "Increase Benefits Taxation") {"<p>Increases the taxation of 
          Social Security benefits </p>
          <p><strong>Open Group Unfunded Obligation</strong> increases from -$10.59 trillion to -$10.93 trillion.</p>
          <p><strong>Insolvency Year</strong> remains unchaged at 2034.</p>"}
      
      else if (input$option == "Cap Spouse Benefits") {"<p>Caps the spouse benefit at $1,121.68 in 2016 beginning for claimants who turn 60 in 2020 and beyond. Indexes the cap annually by chained CPI-U.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$9.94 trillion.</p>
          <p><strong>Insolvency Year</strong> remains unchanged at 2034.</p>"}
      
      else if (input$option == "75% Survivor Benefit") {"<p>Increases joint-and-survivors benefits to 75 percent of combined benefits for the couple, from 50 percent of combined benefits, for claimants who turn 62 in 2022 and beyond.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$9.48 trillion.</p>
          <p><strong>Insolvency Year</strong> remains unchanged at 2034.</p>"}
      
      else if (input$option == "90% Tax Max") {"<p>Raises the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation to cover 90 percent of payroll. This increase is phased in over 10 years, beginning in 2016.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$6.97 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2042.</p>"}
      
      else if (input$option == "90% Tax Max and 13.4% Payroll Tax") {"<p>Raises the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation to cover 90 percent of payroll. This increase is phased in over 10 years, beginning in 2016. Also, increase the payroll tax to 13.4% over t10 years beginning in 2016.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$3.09 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2059.</p>"}
      
      else if (input$option == "Full Chained-CPI COLA") {"<p>Ties beneficiaries' annual cost-of-living-adjustment (COLA) to the change in the chained consumer price index (C-CPI-U), which grows more slowly than the standard CPI-U now used to compute COLAs. (Only those NRA or older)</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.41 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2035.</p>"}
      
      else if (input$option == "Partial Chained-CPI COLA") {"<p>Ties beneficiaries' annual cost-of-living-adjustment (COLA) to the change in the chained consumer price index (C-CPI-U), which grows more slowly than the standard CPI-U now used to compute COLAs. (All beneficiaries including those under the NRA)</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.72 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2035.</p>"}
      
      else if (input$option == "Increase FRA") {"<p>Indefinitely raises Social Security's FRA (now set at 67 beginning in 2022) and the age for receiving delayed retirement credits by one month every two years, beginning in 2024.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.69 trillion.</p>
          <p><strong>Insolvency Year</strong> remains unchanged at 2034.</p>"}
      
      else if (input$option == "Increase FRA and EEA") {"<p>Raises Social Security's early eligibility age (EEA), which is now set at 62, and indefinitely raises Social Security's FRA (now set at 67 beginning in 2022) and the age for receiving delayed retirement credits by one month every two years, beginning in 2024.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.62 trillion.</p>
          <p><strong>Insolvency Year</strong> remains unchanged at 2034.</p>"}
      
      else if (input$option == "$150,000 Tax Max") {"<p>Increase the tax cap to $150,000 between 2016 and 2018 and then increase the tax cap by wage growth plus 0.5 percentage points thereafter.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$9.32 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2035.</p>"}
      
      else if (input$option == "$180,000 Tax Max") {"<p>Increase the tax cap to $180,000 between 2016 and 2018 and then increase the tax cap by wage growth plus 0.5 percentage points thereafter.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$8.76 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2036.</p>"}
      
      else if (input$option == "Eliminate the Tax Max") {"<p>Eliminates the cap on annual earnings subject to the Social Security payroll tax and that enter the benefits calculation.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$4.63 trillion.<br/> <br/>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2055.</p>"}
      
      else if (input$option == "13.4% Payroll Tax") {"<p>Increase the payroll tax rate to 13.4% over 10 years beginning in 2016.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$7.05 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2039.</p>"}
      
      else if (input$option == "14.4% Payroll Tax") {"<p>Increase the payroll tax rate to 14.4% over 10 years beginning in 2016.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$3.53 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2052.</p>"}
      
      else if (input$option == "15.4% Payroll Tax") {"<p>Increase the payroll tax rate to 15.4% over 10 years beginning in 2016.</p>
          <p><strong>Open Group Unfunded Obligation</strong> decreases from -$10.59 trillion to -$0.046 trillion.</p>
          <p><strong>Insolvency Year</strong> increases from 2034 to 2087.</p>"}
      
      else {"<p><strong>Current Law Scheduled</strong> assumes that current public policies, business practices, and individual behaviors continue, and that Social Security benefits are paid as promised, even after the trust fund runs out.</p>
          <p><strong>Current Law Payable</strong> assumes that current public policies, business practices, and individual behaviors continue, but reduces Social Security benefits by a uniform amount after the trust fund runs out so that all benefits in each year can be paid out of revenues from that year.</p>
          <p><strong>Open Group Unfunded Obligation</strong> is -$10.59 trillion.</p>
          <p><strong>Insolvency Year</strong> is 2034.</p>"}
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