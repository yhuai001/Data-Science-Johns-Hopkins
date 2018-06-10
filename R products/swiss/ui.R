library(shiny)

shinyUI(fluidPage(
    titlePanel(h3("Swiss Data Analysis",align = "center")),
    sidebarLayout(
        sidebarPanel(
            textInput("name", "Enter your name!", " "),
            selectInput("columnx", "Select the column for X - axis",
                        choices = c("Fertility" =1,"Agriculture"=2,"Examination" =3,"Education" = 4, "Catholic" = 5,"Infant.Mortality" =6),selected = "Agriculture",selectize = TRUE),
            selectInput("columny", "Select the column for Y - axis",
                        choices = c("Fertility" =1,"Agriculture"=2,"Examination" =3,"Education" = 4, "Catholic" = 5,"Infant.Mortality" =6),selectize = TRUE),
            
            checkboxInput("show_xlab", "Show/Hide X Axis Label", value = TRUE),
            checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = TRUE),
            checkboxInput("show_title", "Show/Hide Title", value = TRUE),
            radioButtons("file", "Selelt the file type", choices = list("pdf", "png")),
            p("Click the download button at the plot tab")
        ),
        mainPanel(
            h5("This Presentation is for : "),
            textOutput("name"),
            tabsetPanel(type = "tab",
                        tabPanel("Data",tableOutput("datatbl")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Modeling", verbatimTextOutput("model")),
                        tabPanel("Plot",plotOutput("plot1"), downloadButton(outputId = "down", label = "download the plot"))
                        
                        
            )
        )
    )))