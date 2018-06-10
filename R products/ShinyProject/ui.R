# Course Project: Shiny Application and Reproducible Pitch
# UI
library(shiny)


shinyUI(fluidPage(
    titlePanel("Stock Analytics"),
    sidebarLayout(
        sidebarPanel(
            helpText("Find out when these stocks have skyrocketed and plunged (for example, the opening or closing price is more than 2% higher than the previous day) from 2018-01-01"),
            h2(helpText("Select:")),
            selectInput("company", label = h3("Company Name"),
                        choices = list("Google" = "GOOG", "Apple" = "AAPL", "Microsoft"="MSFT", "Oracle"="ORCL")),
            checkboxInput("showcorrplot","Show/Hide correlation plot", value=TRUE),
            submitButton("Submit")
        ),

        mainPanel(
            h3("Stock change over 2% start on 2018-01-01"),
            plotOutput("StockPlot"),
            h3("Correlation Plot:"),
            plotOutput("corrlot")
            
        )
    )
))
