library(shiny)

shinyServer(function(input, output) {
    colx <- reactive({
        as.numeric(input$columnx)
    })
    coly <- reactive({
        as.numeric(input$columny)
    })
    output$name <- renderText(input$name)
    output$data <- renderText(input$data)
    output$columnx <- renderText(input$columnx)
    output$columny <- renderText(input$columny)
    
    
    
    output$str <- renderPrint({
        str(swiss)
    })
    output$datatbl <- renderTable({
        #colm <- as.numeric(input$columnx)
        #swiss[colm]
        swiss
        
    })
    output$summary <- renderPrint({
        #colm <- as.numeric(input$columnx)
        #swiss[colm]
        summary(swiss)
        
        
    })
    
    output$model <- renderPrint({
        #colm <- as.numeric(input$columnx)
        #swiss[colm]
        fit <-  lm(swiss[,coly()]~ swiss[,colx()], data = swiss)
        summary(fit)
        
    })
    output$plot1 <- renderPlot({
        #colx <- as.numeric(input$columnx)
        #coly <- as.numeric(input$columny)
        xlab1 <- ifelse(input$show_xlab, paste("X Axis : ",colnames(swiss[colx()])), "")
        ylab1 <- ifelse(input$show_ylab, paste("Y Axis : ",colnames(swiss[coly()])), "")
        main1 <- ifelse(input$show_title, 
                        paste("Relation between", colnames(swiss[colx()]),"and", colnames(swiss[coly()])), "")
        #plot(swiss[,colx()], swiss[,coly()],xlab = xlab,ylab = ylab )
        fit <-  lm(swiss[,coly()]~ swiss[,colx()], data = swiss)
        #abline(fit, col = "blue", lwd = 3)
        scatter.smooth(swiss[,colx()],swiss[,coly()], 
                       lpars = list(lty=2),
                       xlab = xlab1,
                       ylab = ylab1,
                       main = main1)
        abline(fit, col="blue", lwd =3, lty = 3)
        legend(65, 50, pch = c(1, NA, NA), lty = c(NA, 2:3),
               c("Observations", "Loess line", "Regression line"), bty = "n")
    })
    output$down <- downloadHandler(
        filename = function()
        {
            paste("swiss", input$file, sep=".")
        },
        content= function(file){
            #open the device
            #create plot, close the device
            if(input$file == "png")
                png(file)
            else
                pdf(file)
            
            xlab1 <- ifelse(input$show_xlab, paste("X Axis : ",colnames(swiss[colx()])), "")
            ylab1 <- ifelse(input$show_ylab, paste("Y Axis : ",colnames(swiss[coly()])), "")
            main1 <- ifelse(input$show_title, 
                            paste("Relation between", colnames(swiss[colx()]),"and", colnames(swiss[coly()])), "")
            #plot(swiss[,colx()], swiss[,coly()],xlab = xlab,ylab = ylab )
            fit <-  lm(swiss[,coly()]~ swiss[,colx()], data = swiss)
            #abline(fit, col = "blue", lwd = 3)
            scatter.smooth(swiss[,colx()],swiss[,coly()], 
                           lpars = list(lty=2),
                           xlab = xlab1,
                           ylab = ylab1,
                           main = main1)
            abline(fit, col="blue", lwd =3, lty = 3)
            legend(65, 50, pch = c(1, NA, NA), lty = c(NA, 2:3),
                   c("Observations", "Loess line", "Regression line"), bty = "n")
            dev.off()
            
            
        }
    )
    
})