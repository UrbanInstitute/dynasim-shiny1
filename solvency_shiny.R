library(shiny)

ui <- fluidPage(
  
  selectInput(inputId = "Option",
              label = "BPC Option Selector", 
              c("Option 1", "Option 2")),
  
  plotOutput("hist")
  
  
  )

server <- function(input, output) {}

shinyApp(ui = ui, server = server)




# Input Function

# checkboxGroupInput()
# selectInput()

# Ouput Function

