library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  selectInput(inputId = "Option",
              label = "BPC Option Selector", 
              c("Option 1", "Option 2")),
  
  plotOutput("hist")
  
  
  )

server <- function(input, output) {
  output$hist <- renderPlot({ 
    ggplot(cars, aes(x = speed, y = dist)) +
             geom_point()
    })
}

shinyApp(ui = ui, server = server)




# Input Function

# checkboxGroupInput()
# selectInput()

# Ouput Function

