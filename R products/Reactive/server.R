library(shiny)
library(ggplot2)

shinyServer(function(input, output){
    output$result <- renderText({
       
        model <- lm(mpg ~ hp , mtcars)
        pred <- predict(model, newdata = data.frame(hp = input$hp,
                                                    wt = input$wt))
        predmpg <- round(pred,2)
        predmpg
    })

    output$plot1 <- renderPlot({
       
        model <- lm(mpg ~ hp , mtcars)
        pred <- predict(model, newdata = data.frame(hp = input$hp,
                                              wt = input$wt
                                              ))
        
        plot <- ggplot(data=mtcars, aes(x = hp, y = mpg)) +
                    geom_point()+
                    geom_smooth(method = "lm")
        plot
    })
    
    output$plot2 <- renderPlot({
        
        model <- lm(mpg ~ wt , mtcars)
        pred <- predict(model, newdata = data.frame(hp = input$hp,
                                                    wt = input$wt
                                                    ))
        
        plot <- ggplot(data=mtcars, aes(x = wt, y = mpg)) +
            geom_point()+
            geom_smooth(method = "lm")
        plot
    })

})

