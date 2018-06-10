library(shiny)

shinyUI(fluidPage(
    titlePanel("MPG Prediction"),
    sidebarLayout(
        sidebarPanel(
            helpText("This application predicts the Miles/(US) gallon of a car based on its characteristics."),

            selectInput("am", "Transmission (0 = automatic, 1 = manual)", choices = list('Please select'='*', '0'='0','1'='1')),
            sliderInput("hp", "What is the Gross horsepower of the car?", 52, 335, value=100),
            sliderInput("wt", "What is the Weight (1000 lbs) of the car?", 1.5, 5.5, value=3, step = 0.1),
            submitButton("Submit")
            ),
        
        
        mainPanel(
            plotOutput("plot1"),
            plotOutput("plot2"),

            h3("Predicted the car MPG is:"),
            textOutput("result")
    )
  )
))
