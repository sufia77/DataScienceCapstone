library(NLP)
library(tm)
library(stringr)
library(shiny)

options(shiny.maxRequestSize=200*1024^2) # 200 mb

twoGTable <- readRDS("./data2/twoGTable.rds")
threeGTable <- readRDS("./data2/threeGTable.rds")
fourGTable <- readRDS("./data2/fourGTable.rds")
fiveGTable <- readRDS("./data2/fiveGTable.rds")

#source("global.R",  local = TRUE)
source("predict.R",  local = TRUE)

shinyServer(
  function(input, output){
    
    dataInput <- reactive({ 
      predictNextWord6(input$text)
    })
    
    output$text1 <- renderPrint({
      p <- dataInput()[1]
      cat(p)
    })
    
    output$text2 <- renderPrint({
      q <- dataInput()[2]
      cat(q, fill = TRUE)
    })
    
    output$text3 <- renderPrint({
      r <- dataInput()
      cat(r)
    })
  }
)


