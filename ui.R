source('global.R')

shinyUI(fluidPage(

  # Application title
  titlePanel("Neural Network Stock Price Predictor"),

  
    mainPanel(
      plotOutput("stockPlot"),
      helpText("Note: actual values in Blue; predicted values in Red", align = "center")
    )
  )
)
