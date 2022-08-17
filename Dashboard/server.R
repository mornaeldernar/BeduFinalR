#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) {
  #Cargamos los datos
  match.data.csv <- read.csv("./www/match.data.csv", header = TRUE)
  #transformamos en factor el home.team y away.team
  match.data.csv$home.team <- factor(match.data.csv$home.team)
  match.data.csv$away.team <- factor(match.data.csv$away.team)
  
  output$output_text <- renderText(paste("Grafico de goles como ",input$x))
  output$output_plot <- renderPlot({
    x <- match.data.csv[,input$x]
    bin <- seq(min(x), max(x), length.out = 12)
    ggplot(match.data.csv,aes(x,color=away.team))+
      geom_histogram(breaks=bin)+
      facet_wrap(vars(away.team))+
      theme(legend.position="left")+
      labs(x = input$x, y = "Frecuencia") 
  })
  output$data_table <- renderDataTable(match.data.csv,options= list(aLengthMenu= c(10,20,50,100, dim(match.data.csv)[1]),iDisplayLength=10))

})
