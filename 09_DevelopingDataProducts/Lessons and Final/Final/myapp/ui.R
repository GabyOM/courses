#
#

library(shiny)

shinyUI(fluidPage(
    titlePanel("Word predictor app"),
    sidebarLayout(
    sidebarPanel(
        helpText("Enter a partially complete sentence to begin the next word prediction"),
        textInput("inputString", label = h3("Please enter some text"), value = ""),
            
            submitButton("Predict"),
            
            #textOutput("nextwords"),
            
            br()
            
            #h3("Next words can be"),
            
            ),
        
        mainPanel(
            h2("Predicted Next Word"),
            verbatimTextOutput("prediction"),
            strong("Sentence Input:"),
            tabsetPanel(type = "tabs", 
                tabPanel("App Info", 
                      tags$div(br(),
                      tags$p("Shiny application that predicts the next word."),
                      tags$ol(tags$li(h5("Enter text")),
                      tags$li(h5("Click Predict")),
                      tags$li(h5("Most likely words will be displayed"))
                      ),
                      tags$p(h4("Prediction here!"))
                      )
         
                    ))
                )
            )
    ))