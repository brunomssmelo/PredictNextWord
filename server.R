
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

source("probKneserNey.R")
source("preprocessInput.R")

ngramList <- c("pentagrams",
               "quadgrams",
               "trigrams",
               "bigrams",
               "unigrams")

shinyServer(function(input, output) {

  output$table <- DT::renderDataTable(
    DT::datatable(probKneserNey(
      text = preprocessInput(input$text),
      ngramEnv = readRDS("./data/ngramEnv.rds"),
      ngramList = ngramList[which(ngramList == input$highest):length(ngramList)],
      numGuesses = input$guess
    ),
    options = list(paging = FALSE,
                   searching = FALSE,
                   pageLength = input$guess))
  )
  
  
  output$yourSentence <- renderText({
    preprocessInput(input$text)
  })

})
