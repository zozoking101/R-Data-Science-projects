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
      column(width = 6,
      box(
        title = "Select a Number",
        background = "black",
        status="warning",
        width = NULL,
        height = 312,
        sliderInput(inputId = "number",
                    label = "",
                    value = 500, min = 25, max = 1000))),
      
      
      box(
        title = "Histogram",
        solidHeader=TRUE,
        background ="light-blue",
        status="primary",
        width = NULL,
        plotOutput("hist", height = 250))
    ),
    
    column(width = 4,
    valueBoxOutput("meanBox", width = NULL),
    valueBoxOutput("medianBox", width = NULL),
    valueBoxOutput("sdBox", width = NULL)
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
  
  output$meanBox <- renderValueBox({
    valueBox(
      round(mean(histdata()),3),"Mean",
      color = "navy"
    )
  })
  
  output$medianBox <- renderValueBox({
    valueBox(
      round(median(histdata()),3),"Median",
      color = "aqua"
    )
  })
  
  output$sdBox <- renderValueBox({
    valueBox(
      round(sd(histdata()),3), "Standard Deviation",
      color = "blue"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
