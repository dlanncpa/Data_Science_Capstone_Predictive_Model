library(shiny)
suppressMessages(library(shinythemes))

shinyUI(fluidPage(
    theme = shinytheme("flatly"),
    fluidRow(
        column(8, align = "center", offset = 2,
        textInput("text", "Enter Text", width = "100%")
        )
    ),
    fluidRow(
        column(8, align = "center", offset = 2,
        actionButton("button1", label = textOutput("next_word1")),
        actionButton("button2", label = textOutput("next_word2")),
        actionButton("button3", label = textOutput("next_word3"))
        )
    ),
    p("This app predicts the next word of the text typed in the text box. Three available next words
      are produced on three buttons below the text box. If one of the buttons contain the next word you
      would like to use, click the button and the word will be added to the end of the text in the text
      box.")
))
