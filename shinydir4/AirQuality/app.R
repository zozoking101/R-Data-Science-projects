#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyr)   # for drop_na

# safer name than "options" (which would shadow base::options)
choices <- c(
  "Ozone (parts per billion)" = "Ozone",
  "Solar (Langleys)"         = "Solar.R",
  "Wind (MPH)"               = "Wind",
  "Temperature (F)"          = "Temp"
)

# remove rows with any missing values from airquality
aq.no.missing <- drop_na(airquality)

# data frame for mapping the pretty labels to values
df.lv <- data.frame(
  label = names(choices),
  value = unname(choices),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("Air Quality Data"),
  selectInput("X", "X Variable:", choices = choices),
  selectInput("Y", "Y Variable:", choices = choices),
  plotOutput("scatter")
)

server <- function(input, output) {
  
  selections <- reactive({
    req(input$X, input$Y)                     # ensure inputs exist
    aq.no.missing[, c(input$X, input$Y), drop = FALSE]
  })
  
  output$scatter <- renderPlot({
    sel <- selections()
    x_column <- sel[[1]]
    y_column <- sel[[2]]
    
    correlation <- cor(x_column, y_column)
    regression  <- lm(y_column ~ x_column)
    intercept   <- coef(regression)[1]
    slope       <- coef(regression)[2]
    
    X_Label <- df.lv$label[df.lv$value == input$X]
    Y_Label <- df.lv$label[df.lv$value == input$Y]
    
    main_text <- paste0(
      Y_Label, " vs ", X_Label,
      "\n r = ", round(correlation, 3),
      "    Y' = ", round(intercept, 3), " + ", round(slope, 3), " X"
    )
    
    plot(
      x = x_column, y = y_column,
      xlab = X_Label, ylab = Y_Label,
      cex.axis = 1.2, cex.lab = 1.3, pch = 20, cex = 1.2,
      main = main_text
    )
    abline(a = intercept, b = slope, col = "blue", lwd = 2)
  })
}

shinyApp(ui = ui, server = server)
