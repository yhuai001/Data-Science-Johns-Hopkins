library(shiny)

# Select columns to be used
data <- read.csv('CreditData.csv', stringsAsFactors = TRUE)
data <- data[,c(1:4,12:15,19)]

# Define server logic
shinyServer(function(input, output) {

    output$result <- renderText({
        
        fit <- glm(fgood~., data)
        pred <- predict(fit, newdata = data.frame(cbs1 = input$car,
                                                  cbs2 = input$cut,
                                                  cbs3 = input$col,
                                                  tob = input$clar,
                                                  dep = input$dep,
                                                  inc = input$inc,
                                                  dpd = input$dpd,
                                                  pmt = input$pmt))
        res <- ifelse(pred == 1, "Good!","Not Good!")
        res
    })
    
})