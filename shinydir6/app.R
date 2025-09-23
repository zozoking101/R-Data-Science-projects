#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(MASS)   # for Cars93
library(dplyr)

# numeric variables only
num_vars <- Cars93 %>%
  select_if(is.numeric) %>%
  colnames()

# create label/value pairs: values are real column names, labels are human-readable
choices <- setNames(num_vars, gsub("\\.", " ", num_vars))

ui <- fluidPage(
  titlePanel("Cars93 Data"),
  selectInput("X", "X Variable:", choices = choices),
  selectInput("Y", "Y Variable:", choices = choices),
  plotOutput("scatter")
)

server <- function(input, output) {
  
  selections <- reactive({
    req(input$X, input$Y)
    Cars93[, c(input$X, input$Y), drop = FALSE]
  })
  
  output$scatter <- renderPlot({
    sel <- selections()
    
    # directly use column names
    x_column <- sel[[input$X]]
    y_column <- sel[[input$Y]]
    
    # correlation and regression
    correlation <- cor(x_column, y_column, use = "complete.obs")
    regression  <- lm(y_column ~ x_column)
    intercept   <- coef(regression)[1]
    slope       <- coef(regression)[2]
    
    # pretty labels (from choices)
    X_Label <- names(choices)[choices == input$X]
    Y_Label <- names(choices)[choices == input$Y]
    
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
        axis.text.x  = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y  = element_text(size = 16),
        plot.title   = element_text(hjust = 0.5, size = 20)
      )
  })
}

shinyApp(ui = ui, server = server)
