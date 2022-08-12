match.data.csv <- read.csv("www/match.data.csv", header = TRUE)
colnames(match.data.csv)
head(match.data.csv)
summary(match.data.csv)
match.data.csv$home.team <- factor(match.data.csv$home.team)
match.data.csv$away.team <- factor(match.data.csv$away.team)
plot <- ggplot(match.data.csv, aes(x=away.score,y=home.team))+
  geom_bar(stat="identity")+
  facet_wrap(vars(away.team));
plot


library(shiny)
match.data.csv <- read.csv("www/match.data.csv", header = TRUE)
match.data.csv$home.team <- factor(match.data.csv$home.team)
match.data.csv$away.team <- factor(match.data.csv$away.team)

shinyServer(function(input, output) {
  output$output_text <- renderText(paste("Grafico de ",input$x," ~ ",input$y))
  output$output_plot <- renderPlot({
    color <- c("blue","red")
    our_data=match.data.csv
    barplot(colSums(our_data[,c("home.score","away.score")]),
            ylab="Team",
            xlab="score",
            name.arg = c("away.team","away.score"),
            col = color
    )
  }  
  )
  output$data_table <- renderDataTable(match.data.csv,options= list(aLengthMenu= c(10,20,50),iDisplayLength=10))
  
})
