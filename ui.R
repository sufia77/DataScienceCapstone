#install.packages("shiny")
library(shiny)

shinyUI(fluidPage(
  titlePanel("Next-Word Prediction: Simple SwiftKey"),
  
  navlistPanel(
    tabPanel("Word Prediction", icon = icon("keyboard-o"),
             textInput("text", "Enter Text Below", width = 550),
             submitButton('Submit'),
             mainPanel(
               h4(div("Suggested Current Word:",style = "color:blue")),
               h4(textOutput("text1")),
               h4(div("Possible Next Word:",style = "color:Green")),
               h4(textOutput("text2")),
               h4(div("Top Three Words:",style = "color:red")),
               h4(textOutput("text3"))),
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             )),
    tabPanel("User Guide",icon = icon("user"),
             h2("Welcome to the Word Predictor!"),
             h3("Instructions:"),
             p(" This app will take approximately a minute for data to be loaded initially."),
             p("Select Word Prediction from navigation panel. Type text and click on the submit button, 
               this app will give you the next three words based on the frequency in the text
               corpus. You can type any number of words to get the next possible
               word. You don't need to use any quotation mark in the text."),
             h3("Results:"),
             p("This app will report three posible next words. If text database doesn't have enough
               words to predict, it will show a message that \"words not found in database\"."  )),
    tabPanel("Documentation", icon = icon("folder"),
             div(" This app is part of final assignment of Coursera Data Science
                 Capstone Project.The original data is from a corpus called
                 HC Corpora (www.corpora.heliohost.org)."),
             h3("Prediction Model"),
             div("N-gram model was used to predict next words. This app uses 5-grams
                 to 2-grams to predict words based on",
                 a(href = "http://www.aclweb.org/anthology/D07-1090.pdf",
                   target="_blank", "Stupid backoff model."),"Low frequency words
                 were removed from the n-grams to make the app faster."),
             h3("Research"),
             p("Different models were investigated for this app such as
               Katz's back-off model, Stupid backoff model and Kneser-Ney smoothing.
               It is found from the study that stupid backoff model is much easier
               and simpler for the web app if there are limited data in the database.
               If the database is large enough then the maximum likelihood estimate and
               stupid backoff model gives similar results.")),
    tabPanel("References", icon = icon("book"),
             h3("References"),
             p(a(href = "https://en.wikipedia.org/wiki/Natural_language_processing",
                 target="_blank", "Natural language processing Wikipedia page")),
             p(a(href = "https://www.jstatsoft.org/article/view/v025i05",
                 target="_blank", "Text mining infrastucture in R")),
             p(a(href = "https://cran.r-project.org/web/views/NaturalLanguageProcessing.html",
                 target="_blank", "CRAN Task View: Natural Language Processing" )),
             p(a(href = "http://www.aclweb.org/anthology/D07-1090.pdf",
                 target="_blank", "Stupid backoff model."))),
    tabPanel("Contact Info", icon = icon("envelope"),
             h3("Contact Info"),
             p("If you would like to know more about this study and app,
               you can email me via linkedin."),
             p("Dr. Sufia Khatun"),
             p(a(href = "https://www.linkedin.com/in/sufia-khatun-19046824",
                 target="_blank", "Linkedin" )))
    ),
  fluidRow(
    column(8,    
           h3("SwiftKey Data Science Capstone Project:"),
           div("The goal of this study was to process the text data provided by SwiftKey, 
               perform exploratory analysis, and prepare the text data to build model for
               predicting next word. Details of this study are shown in the following link:",
               a(href="http://www.rpubs.com/sufia77/205846",
                 target="_blank", "Milestone Report"), style = "color:black")
           
    )
  )
 )
)




