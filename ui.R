library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("superhero"),
  titlePanel("Next Word Prediction App"),
  ##Simple UI layout with entry field in the sidebar and output in the main panel.
  sidebarLayout(
    sidebarPanel(
        h4("This application takes any text input and predicts the most likely next word in the sequence"),
        textInput("text", label = "Enter text here", value = "", width = NULL, placeholder = "Your words here")
        ),
    ##Main panel outputs concatenated sentence with input + predicted word as well as wordcloud of five most likely words.
    mainPanel(
        h4("Is this what you wanted to write?"),
        tags$blockquote(textOutput("MostLikelySentence")),
        tags$hr(),
        h4("These are the five most likely next words"),
        plotOutput("wordcloud", height = "300px")
    )
  )
))
