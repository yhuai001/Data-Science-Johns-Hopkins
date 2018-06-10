library(shiny)

shinyUI(fluidPage(
    
    # Title
    titlePanel("Credit Status Prediction"),
    
    # Sidebar with options selectors 
    sidebarLayout(
        sidebarPanel(
            helpText("This application will predicts your credit status is good or not"),
            h3(helpText("Please Select:")),
            numericInput("cbs1", label = h4("Credit score 1"), value = 640),
            numericInput("cbs2", label = h4("Credit score 2"), value = 640),
            numericInput("cbs3", label = h4("Credit score 3"), value = 640),
            numericInput("tob", label = h4("Time on books (Years)"), step=1, value = 3),
            numericInput("dep", label = h4("Amount of deposits own by customer"), value = 10000),
            
            numericInput("inc", label = h4("Level of income(W01-W10)"), step=1, value = 5),
            
            selectInput("dpd", label = h4("Level of delinquency (No, Low, High)"), 
                        choices = list("Unknown" = "*", "No" = 1, "Low" = 2,
                                       "High" = 3)),
            selectInput("pmt", label = h4("Type of payment (M: Manual, A: Autopay, P: Payroll)"), 
                        choices = list("Unknown" = "*", "M" = 1, "A" = 2,
                                       "P" = 3))
        ),
        submitButton("Submit"),
        # Show results
        mainPanel(
            h4("Your credit status is:"),
            h3(textOutput("result"))
        )
    )
))