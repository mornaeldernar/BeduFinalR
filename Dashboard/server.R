#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  match.data.csv <- read.csv("www/match.data.csv", header = TRUE)
  output$data_table <- renderDataTable(match.data.csv,options= list(aLengthMenu= c(10,20,50),iDisplayLength=10))

})
