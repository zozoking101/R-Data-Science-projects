#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shinydashboard)

# Define UI for dashboard application for the histogram
ui <- dashboardPage(
  dashboardHeader(title = "Uniform Distribution"),
  dashboardSidebar(),
  dashboardBody(
    
    fluidRow(
      box(
        title = "Select a Number",
        status="warning",
        sliderInput(inputId = "number",
                    label = "",
                    value = 500, min = 25, max = 1000)),
      
      box(
        title = "Histogram",
        status="primary",
        plotOutput("hist", height = 250))
    )
    
  )
)

# Define server logic required to plot the histogram
server <- function(input, output) {
  
  histdata <- reactive({runif(input$number,min=0,max=1)})
  output$hist <- renderPlot({
    
    hist(histdata(),xlab="Value",
         main=paste(input$number,"random values between 0 and 1"))
  })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
