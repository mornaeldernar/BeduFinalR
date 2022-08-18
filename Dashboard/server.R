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
library(dplyr)                             # Install and load formattable
                                 # Install and load scales
library("scales")

shinyServer(function(input, output) {
  #Cargamos los datos
  match.data.csv <- read.csv("./www/match.data.csv", header = TRUE)
  #transformamos en factor el home.team y away.team
  match.data.csv$home.team <- factor(match.data.csv$home.team)
  match.data.csv$away.team <- factor(match.data.csv$away.team)
  match.data.csv.win <- match.data.csv %>% 
      mutate(Ganador = case_when(home.score - away.score > 0 ~ "Local",home.score - away.score < 0 ~ "Visitante",TRUE ~ "Empate"))
  visitantes <- length(match.data.csv.win[match.data.csv.win$Ganador == "Visitante",1])
  locales <- length(match.data.csv.win[match.data.csv.win$Ganador == "Local",1])
  empates <- length(match.data.csv.win[match.data.csv.win$Ganador == "Empate",1])
  
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
  output$win_visitante <- renderText({
    scales::percent(round(visitantes / (visitantes+empates+locales),2))
  })
  output$win_local <- renderText({
    scales::percent(round(locales  / (visitantes+empates+locales),2))
  })
  output$win_empate <- renderText({
    scales::percent(round(empates  / (visitantes+empates+locales),2))
  })
  output$data_table <- renderDataTable(match.data.csv.win,options= list(aLengthMenu= c(10,20,50,100, dim(match.data.csv)[1]),iDisplayLength=10))

})
