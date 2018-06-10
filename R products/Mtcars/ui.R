# This is the UI R Program which displys the input criteria to select the car. Uses 2 Panel :
# Panel 1 : To display the search criteria
# Panel 2 : To display the output based on the selection

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Finding the Best Car for the Trip"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            helpText("Please provide your car preference"),
            
            numericInput('distance', 'Distance to Travel ( In Miles ):', 20, min = 1, max = 1000),
            
            numericInput('cost', 'Gasoline Price ( per Galon ):', 2.5, min = 1.5, max = 5, step=0.1),
            
            numericInput('gas', 'Maximum gasoline Expenditure:', 100, min=1, max=1000),
            
            checkboxGroupInput('cyl', 'Number of cylinders:', c("Four"=4, "Six"=6, "Eight"=8), selected = c(6,8)),
            
            sliderInput('disp', 'Displacement ( cu.in ) ', min=70, max=480, value=c(70,480), step=10),
            
            sliderInput('hp', 'HorsePower', min=50, max=340, value=c(50,340), step=10),
            
            checkboxGroupInput('am', 'Transmission Mode:', c("Automatic"=0, "Manual"=1), selected = c(0,1))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput('table')
        )
    )
))