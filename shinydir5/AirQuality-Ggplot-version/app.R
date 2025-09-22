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
library(ggplot2)

# safer name than "options" (which would shadow base::options)
choices <- c(
  "Ozone (parts per billion)" = "Ozone",
  "Solar (Langleys)"          = "Solar.R",
  "Wind (MPH)"                = "Wind",
  "Temperature (F)"           = "Temp"
)

# remove rows with missing values
aq.no.missing <- drop_na(airquality)

# data frame for mapping pretty labels to variable names
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
    req(input$X, input$Y) # ensure inputs exist
    aq.no.missing[, c(input$X, input$Y), drop = FALSE]
  })
  
  output$scatter <- renderPlot({
    sel <- selections()
    x_column <- sel[[1]]
    y_column <- sel[[2]]
    
    # compute stats
    correlation <- cor(x_column, y_column)
    regression  <- lm(y_column ~ x_column)
    intercept   <- coef(regression)[1]
    slope       <- coef(regression)[2]
    
    # labels
    X_Label <- df.lv$label[df.lv$value == input$X]
    Y_Label <- df.lv$label[df.lv$value == input$Y]
    
    # main plot
    ggplot(sel, aes_string(x = input$X, y = input$Y)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", col = "black") +
      labs(
        x = X_Label,
        y = Y_Label,
        title = paste(
          Y_Label, "vs", X_Label,
          "\n r =", round(correlation, 3),
          "   Y' =", round(intercept, 3), "+", round(slope, 3), "X"
        )
      ) +
      theme(
        axis.title.x = element_text(size = 18),
        axis.text.x  = element_text(size = 17),
        axis.title.y = element_text(size = 18),
        axis.text.y  = element_text(size = 17),
        plot.title   = element_text(hjust = 0.5, size = 20)
      )
  })
}

shinyApp(ui = ui, server = server)
