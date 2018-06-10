# Server program for shiny app which will be receive the input & send the output back. 

library(shiny)
library(datasets)
library(dplyr)

# Server logic to display the cars that meet the criteria provided for the trip
shinyServer(function(input, output) {
    
    
    output$table <- renderDataTable({
        
        disp_seq <- seq(from = input$disp[1], to = input$disp[2], by = 1)
        
        hp_seq <- seq(from = input$hp[1], to = input$hp[2], by = 1)
        
        data <- transmute(mtcars, Car = rownames(mtcars), MilesPerGallon = mpg, 
                          GasExpenditure = input$distance/mpg*input$cost,
                          Cylinders = cyl, Displacement = disp, Horsepower = hp, 
                          Transmission = am)
        
        data <- filter(data, GasExpenditure <= input$gas, Cylinders %in% input$cyl, 
                       Displacement %in% disp_seq, Horsepower %in% hp_seq, Transmission %in% input$am)
        
        data <- mutate(data, Transmission = ifelse(Transmission==0, "Automatic", "Manual"))
        
        data <- arrange(data, GasExpenditure)
        
        data
        
        
    }, options = list(lengthMenu = c(10, 20, 30), pageLength = 30))
    
    
})
