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
    )
))
