
library(dplyr)
library(ggplot2)

setwd("D:/Prohoff/git/BeduFinalR/Dashboard")
match.data.csv <- read.csv("./www/match.data.csv", header = TRUE)
match.data.csv$home.team <- factor(match.data.csv$home.team)
match.data.csv$away.team <- factor(match.data.csv$away.team)
typeof(match.data.csv)
head(match.data.csv)
data <- mutate(match.data.csv, FTR = ifelse(home.score > away.score, "H", ifelse(home.score < away.score,"A","D")))

head(data)
typeof(data)
x <- data[,"away.score"]
head(x)
head(data["away.score"])
data %>% ggplot(aes(home.team,x))+
  geom_bar(stat="identity")+
  facet_wrap(vars(away.team))+
  labs(x = "input$x", y = "Goles") +
  ylim(0,10)
