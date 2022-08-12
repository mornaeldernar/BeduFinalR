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
match.data.csv <- read.csv("www/match.data.csv", header = TRUE)
match.data.csv$home.team <- factor(match.data.csv$home.team)
match.data.csv$away.team <- factor(match.data.csv$away.team)

shinyServer(function(input, output) {
  plot <- ggplot(match.data.csv, aes(x=input$x,y=home.team))+
    geom_bar(stat="identity")+
    facet_wrap(vars(away.team));
  output$output_text <- renderText(paste("Grafico de goles como ",input$x))
  output$output_plot <- renderPlot(
    plot
  )
  output$data_table <- renderDataTable(match.data.csv,options= list(aLengthMenu= c(10,20,50),iDisplayLength=10))

})
