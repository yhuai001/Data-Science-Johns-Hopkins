library(shiny)

shinyUI(fluidPage(
    
    # Title
    titlePanel("Credit Status Prediction"),
    
    # Sidebar with options selectors 
    sidebarLayout(
        sidebarPanel(
            helpText("This application will predicts your credit status is good or not"),
            h3(helpText("Please Select:")),
            numericInput("car", label = h4("Carats"), step = 0.01, value = 2),
            selectInput("cut", label = h4("Cut"), 
                        choices = list("Unknown" = "*", "Fair" = "Fair", "Good" = "^Good",
                                       "Very Good" = "Very Good", "Premium" = "Premium",
                                       "Ideal" = "Ideal")),
            selectInput("col", label = h4("Color"), 
                        choices = list("Unknown" = "*", "D" = "D", "E" = "E",
                                       "F" = "F", "G" ="G",
                                       "H" = "H", "I" = "I",
                                       "J" = "J")),
            selectInput("clar", label = h4("Clarity"), 
                        choices = list("Unknown" = "*", "I1" = "I1", "SI2" = "SI2",
                                       "SI1" = "SI1", "VS2" = "VS2", "VS1" = "VS1",
                                       "VVS2" = "VVS2", "VVS1" = "VVS1", "IF" = "IF" ))
        ),
        
        # Show results
        mainPanel(
            h4("Your credit status is:"),
            h3(textOutput("result"))
        )
    )
))