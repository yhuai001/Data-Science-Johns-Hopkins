# Course Project: Shiny Application and Reproducible Pitch
# Server

library(shiny)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(quantmod)
library(psych)
library(corrplot)

# Get stock data
stock <- getSymbols(c("AAPL", "ORCL", "MSFT", "GOOG"), src = "yahoo", env = new.environment, from = "2018-01-01", to = "2018-05-24")

# Prepare the correaltion plot
m <- cbind(Ad(get("AAPL", env = new.environment)), Ad(get("ORCL", env = new.environment)), Ad(get("MSFT", env = new.environment)), Ad(get("GOOG", env = new.environment)))

# Define server logic required to draw a plot
shinyServer(function(input, output) {
    output$StockPlot <- renderPlot({
        
        choice <- reactive({
            choice <- Delt(Cl(get(input$company)))
        })
        
        plot <- plot(choice[which(choice > 0.02), ])
        plot
    })
    
    if(input$showcorrplot){
        corr.test(as.data.frame(m))
        corrplot.mixed(cor(m), lower = "square", upper = "circle")
    }
    
    output$corrlot <- renderPlot({
        # renders the correlation plot if customer choose 'show'
        corrplot <- corrplot.mixed(cor(m), lower = "square", upper = "circle")
        corrlot
    })
    
})

