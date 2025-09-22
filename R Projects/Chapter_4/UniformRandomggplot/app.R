#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(ggplot2)
library(shiny)

ui <- fluidPage(
  sliderInput(
    inputId = "number",
    label = "Select a number",
    value = 500,
    min = 1,
    max = 1000
  ),
  sliderInput(
    inputId = "binwidth",
    label = "Select a binwidth",
    value = 0.05,
    min = 0.01,
    max = 0.10,
    step = 0.01
  ),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    df <- data.frame(Value = runif(input$number, min = 0, max = 1))
    
    ggplot(df, aes(x = Value)) +
      geom_histogram(
        binwidth = input$binwidth,
        color = "black",
        fill = "grey80"
      ) +
      labs(
        y = "Frequency",
        title = paste(
          input$number,
          "random values from 0 to 1 with binwidth =",
          input$binwidth
        )
      )
  })
}

shinyApp(ui = ui, server = server)
