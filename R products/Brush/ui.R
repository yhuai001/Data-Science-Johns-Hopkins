library(shiny)

shinyUI(fluidPage(
    titlePanel("Visualize Many Models"),
    sidebarLayout(
        sidebarPanel(
            h3("Slope"),
            textOutput("splopeOut"),
            h3("Intercept"),
            textOutput("intOut")
            
        ),
        mainPanel(
            plotOutput("plot1", brush = brushOpts(
                id="brush1"
            ))))))