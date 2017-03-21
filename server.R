source("global.R")
##Require the algorithm function in separate R script
source("stupid_backoff.R")

shinyServer(function(input, output) {
        prediction <- reactive ({
        inputText <- input$text
        prediction <- predict(inputText)
        })
        ##This outputs the concatenated text "input + most likely next word"
        ##If no input is entered, it returns a message
        ##If word cannot be recognized, it returns an error message
        output$MostLikelySentence <- renderText(
                if (input$text == "") { 
                        "Please enter some text on the left"
                        }
                else if (anyNA(prediction()$LastWord)) { 
                        "I'm sorry I don't recognize this input"
                        } else {
                        paste(as.character(input$text), as.character(prediction()$LastWord[1]))
                        })
        ##This outputs the top 5 predicted words as a word cloud
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
