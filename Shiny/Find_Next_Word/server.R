library(shiny)

source("find_next_word.r")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  observe({
      textEnter<-reactive({input$text})
      
      next_word<-find_next_word(textEnter())
      a1<<-next_word[1]
      a2<<-next_word[2]
      a3<<-next_word[3]

      output$next_word1<-reactive({
          next_word[1]
      })
      
      output$next_word2<-reactive({
          next_word[2]
      })
      
      output$next_word3<-reactive({
          next_word[3]
      })
      
  })
      
      observeEvent(input$button1, {
          name1<-paste(input$text, a1)
          updateTextInput(session, "text", value = name1)
      })
      
      observeEvent(input$button2, {
          name2<-paste(input$text, a2)
          updateTextInput(session, "text", value = name2)
      })
      
      observeEvent(input$button3, {
          name3<-paste(input$text, a3)
          updateTextInput(session, "text", value = name3)
      })
})
