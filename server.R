source("global.R")
source("stupid_backoff.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        prediction <- reactive ({
        inputText <- input$text
        prediction <- predict(inputText)
        })
        output$MostLikelySentence <- renderText(
                if (input$text == "") { 
                        "Please enter some text on the left"
                        }
                else if (anyNA(prediction()$LastWord)) { 
                        "I'm sorry I don't recognize this input"
                        } else {
                        paste(as.character(input$text), as.character(prediction()$LastWord[1]))
                        })
        output$wordcloud <- renderPlot(
                if (anyNA(prediction()$LastWord)) {
                        NULL
                } else {
                        wordcloud(prediction()$LastWord,
                        prediction()$scores, 
                        scale = c(6,1),
                        rot.per = FALSE, 
                        colors = brewer.pal(5, "Set1"))
                                       
                })
})
