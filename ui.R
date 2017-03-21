library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("superhero"),
    # Application title
  titlePanel("Next Word Prediction App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        h4("This application takes any text input and predicts the most likely next word in the sequence"),
        textInput("text", label = "Enter text here", value = "", width = NULL, placeholder = "Your words here")
        ),
    mainPanel(
        h4("Is this what you wanted to write?"),
        tags$blockquote(textOutput("MostLikelySentence")),
        tags$hr(),
        h4("These are the five most likely next words"),
        plotOutput("wordcloud", height = "300px")
    )
  )
))
