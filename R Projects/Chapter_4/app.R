#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
  sliderInput(
    inputId = "bins",
    label = "Number of bins",
    min = 1,
    max = 50,
    value = 30
  ),
  plotOutput("distPlot")
)

server <- function(input, output) {
  
  # reactive expression to generate the data and bins
  histdata <- reactive({
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    list(x = x, bins = bins)
  })
  
  output$distPlot <- renderPlot({
    hist(histdata()$x,
         breaks = histdata()$bins,
         col = 'darkgray',
         border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

shinyApp(ui = ui, server = server)
