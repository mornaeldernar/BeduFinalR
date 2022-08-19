install.packages("remotes")
library(remotes)
install_github("cran/fbRanks")
library(fbRanks)
library(dplyr)
library(ggplot2)



# Colocar el directorio de trabajo según corresponda

setwd("D:/Prohoff/git/BeduFinalR")

# Descarga de archivos
# https://www.football-data.co.uk/spainm.php

u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"
u1415 <- "https://www.football-data.co.uk/mmz4281/1415/SP1.csv"
u1516 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
u1617 <- "https://www.football-data.co.uk/mmz4281/1617/SP1.csv"
u1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
u1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

#RawData <- "C:\\\"
download.file(url = u1011, destfile ="data/SP1-1011.csv", mode = "wb")
download.file(url = u1112, destfile ="data/SP1-1112.csv", mode = "wb")
download.file(url = u1213, destfile ="data/SP1-1213.csv", mode = "wb")
download.file(url = u1314, destfile ="data/SP1-1314.csv", mode = "wb")
download.file(url = u1415, destfile ="data/SP1-1415.csv", mode = "wb")
download.file(url = u1516, destfile ="data/SP1-1516.csv", mode = "wb")
download.file(url = u1617, destfile ="data/SP1-1617.csv", mode = "wb")
download.file(url = u1718, destfile ="data/SP1-1718.csv", mode = "wb")
download.file(url = u1819, destfile ="data/SP1-1819.csv", mode = "wb")

# Lectura de datos

setwd("D:/Prohoff/git/BeduFinalR/data")
lista <- lapply(list.files(path = "D:/Prohoff/git/BeduFinalR/data"), read.csv)

download.file(url = u1920, destfile ="SP1-1920.csv", mode = "wb")

# Procesamiento de datos

#lista <- lapply(lista, select, Date:FTR, BbMx.2.5:BbAv.2.5.1)

d1011 <- read.csv("SP1-1011.csv")
d1112 <- read.csv("SP1-1112.csv")
d1213 <- read.csv("SP1-1213.csv")
d1314 <- read.csv("SP1-1314.csv")
d1415 <- read.csv("SP1-1415.csv")
d1516 <- read.csv("SP1-1516.csv")
d1617 <- read.csv("SP1-1617.csv")
d1718 <- read.csv("SP1-1718.csv")
d1819 <- read.csv("SP1-1819.csv")
d1920 <- read.csv("SP1-1920.csv")

d1011 <- select(d1011, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1112 <- select(d1112, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1213 <- select(d1213, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1314 <- select(d1314, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1415 <- select(d1415, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1516 <- select(d1516, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1617 <- select(d1617, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1718 <- select(d1718, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1819 <- select(d1819, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1920 <- select(d1920, Date:FTAG, Max.2.5:Avg.2.5.1)
d1920 <- select(d1920, -Time)
colnames(d1920)

# Arreglamos las fechas
d1011 <- mutate(d1011, Date = as.Date(Date, format = "%d/%m/%y"))
d1112 <- mutate(d1112, Date = as.Date(Date, format = "%d/%m/%y"))
d1213 <- mutate(d1213, Date = as.Date(Date, format = "%d/%m/%y"))
d1314 <- mutate(d1314, Date = as.Date(Date, format = "%d/%m/%y"))
d1415 <- mutate(d1415, Date = as.Date(Date, format = "%d/%m/%y"))
d1516 <- mutate(d1516, Date = as.Date(Date, format = "%d/%m/%y"))
d1617 <- mutate(d1617, Date = as.Date(Date, format = "%d/%m/%y"))
d1718 <- mutate(d1718, Date = as.Date(Date, format = "%d/%m/%y"))
d1819 <- mutate(d1819, Date = as.Date(Date, format = "%d/%m/%Y"))
d1920 <- mutate(d1920, Date = as.Date(Date, format = "%d/%m/%Y"))

# Unimos de d1415S a d1819S

d1019 <- rbind(d1011, d1112, d1213, d1314, d1415, d1516, d1617, d1718, d1819)

# Renombrar columnas
d1019 <- rename(d1019,  Max.2.5.O = BbMx.2.5, 
                 Avg.2.5.O = BbAv.2.5, 
                 Max.2.5.U = BbMx.2.5.1,
                 Avg.2.5.U = BbAv.2.5.1)

d1920 <- rename(d1920,  Max.2.5.O = Max.2.5, 
                 Avg.2.5.O = Avg.2.5, 
                 Max.2.5.U = Max.2.5.1,
                 Avg.2.5.U = Avg.2.5.1)

# Ordenamos las columnas

d1019 <- select(d1019, colnames(d1920))

# Volvemos a unir

d1020S <- rbind(d1019, d1920)

# Renombramos

d1020S <- rename(d1020S, date = Date, home.team = HomeTeam, home.score = FTHG, away.team = AwayTeam, away.score = FTAG)

# Ordenamos columnas

data <- select(d1020S, date, home.team, home.score, away.team, away.score:Avg.2.5.U) # Este data frame contiene todos los datos necesarios

head(data, n = 2L); tail(data, n = 2L)

# Data frames de partidos y equipos
setwd('D:/Prohoff/git/BeduFinalR')
md <- data %>% select(date:away.score)
write.csv(md, "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")
teams <- df$teams; scores <- df$scores

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)
####
#### Tenemos 33 equipos y 3800 partidos desde el 2010 hasta el 2020
####

# Conjuntos iniciales de entrenamiento y de prueba
f <- scores$date # Fechas de partidos
fu <- unique(f) # Fechas sin repetición
#####
##### Hubo 1236 fechas en las que hubo partidos( en algunas fechas hubo varios partidos)
#####
Ym <- format(fu, "%Y-%m") # Meses y años
Ym <- unique(Ym) # Meses y años sin repetir
####
#### Hubo partidos por 101 meses
####
format(scores$date, "%Y-%m")

### No hay partidos entre los mese 06 y 07 en 2011 y 2012, 2014, 2015, 2016, 2017, 2018, 2019, ,
### en 2013 no hay partidos en en el mes 07
### en 2020 no hubo partidos en 04 y 05

## Aqui se seleccionan los partidos de diciembre 2011-08 que fue el 15avo mes que jugaron
places <- which(Ym[15]==format(scores$date, "%Y-%m")) # Consideramos partidos de 15 meses para comenzar a ajustar el modelo
ffe <- scores$date[max(places)] # Fecha final conjunto de entrenamiento
### se jugó en diciembre 2011 hasta el dia 18


# Consideraremos partidos de 15 meses para comenzar a ajustar el modelo. Así, nuestro primer conjunto de entrenamiento consiste de datos de partidos hasta el `r ffe` 
train <- scores %>% filter(date <= ffe) ## entrenamos desde el inicio del dataset hasta el 18 de dic de 2011
test <- scores %>% filter(date > ffe)   ## los datos posteriores a esa fecha, seran para el test

head(train, n = 1); tail(train, n = 1)
head(test, n = 1); tail(test, n = 1)

# Primer ajuste del modelo

traindate <- unique(train$date)
testdate <- unique(test$date)

ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])
ranks
coef(ranks)
#######
####### le da una puntuacion al equipo por ataque defensa y total, y numero de partidos


# Primera predicción
testdate[1]
pred <- predict(ranks, date = testdate[1])

phs <- pred$scores$pred.home.score # predicted home score
pas <- pred$scores$pred.away.score # predicted away score
pht <- pred$scores$home.team # home team in predictions
pat <- pred$scores$away.team # away team in predictions
phs
pas
pht
pat

# Continuar ajustando y prediciendo

phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL
for(i in 1:(length(unique(scores$date))-170)){
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005) ## 0 no weight, 0.1 los juegos mas recientes pesan mas, 0.0005 los juegos mas recientes no pesan tanto
  pred <- predict(ranks, date = unique(scores$date)[i+170],
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score) # predicted home score
  pas <- c(pas, pred$scores$pred.away.score) # predicted away score
  pht <- c(pht, pred$scores$home.team) # home team in predictions
  pat <- c(pat, pred$scores$away.team) # away team in predictions
}
phs
pas
pht
pat

# Eliminamos NA's

buenos <- !(is.na(phs) | is.na(pas)) # 
phs <- phs[buenos] # predicted home score
pas <- pas[buenos] # predicted away score
pht <- pht[buenos] # home team in predictions
pat <- pat[buenos] # away team in predictions
fecha_prueba <- unique(scores$date)[171]

momio <- data %>% filter( date >= fecha_prueba) # momios conjunto de prueba
momio <- momio[buenos,]
mean(pht == momio$home.team); mean(pat == momio$away.team) ## da 1 porque son las medias de los equipos que jugaron????
mean(pht == momio$home.score); mean(pat == momio$away.score)
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)
hs <- momio$home.score
as <- momio$away.score

# Probabilidades condicionales
mean(phs + pas > 3) # proporción de partidos con más de tres goles según el modelo
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) # probabilidad condicional estimada de ganar en over 2.5

mean(phs + pas < 2.1) # proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) # probabilidad condicional estimada de ganar en under 2.5

# Juegos con momios máximos

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}


# Escenario con momios máximos
g
g <- data.frame(Num_Ap = 1:length(g), Capital = g)

df_means <- dataframe(mean(g$Capital))
df_means$mean <- (mean(g$Capital))
df_means
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) +
  geom_line( color="green") +
  geom_point( color="darkgreen") +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  geom_hline(data=df_means, aes(yintercept=mean, color="Promedio"), linetype="dashed")+
  
  theme(legend.position="none")+
  theme(axis.text.x = element_text(face = "bold", color="darkgreen" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="darkgreen" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p

mean(g$Capital) #El capital promedio invertido en maximos es de 44881.79

# Escenario con momios promedio

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}
library(plyr)
g <- data.frame(Num_Ap = 1:length(g), Capital = g)
df_means$mean <- (mean(g$Capital))
df_means 
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + 
  geom_line( color="green") + 
  geom_point(color="darkgreen") +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +

  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="darkgreen" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="darkgreen" , size = 10, angle = 25, hjust = 1))+
  geom_hline(data=df_means, aes(yintercept=mean, color="Promedio"), linetype="dashed")+
  
  theme(legend.position="none")
# color, ángulo y estilo de las abcisas y ordenadas 
p

mean(g$Capital) #El capital promedio invertido en promedio es de 29816.94

#Paso 1 Planteamiento de hipotesis
# h0 
# h1
hip <- 20000
#Paso 2 Calcular estadistico de prueba
media <- mean(g$Capital)
ds <- sd(g$Capital)
n <- length(g$Capital)
t <- (media - hip)/(ds/sqrt(n))

gl <- n-1
#Paso 3: Calcular P value
pvalue <- pt(t,df = gl, lower.tail = T)
pvalue

#Paso 4: seleccionar nivel de confianza y concluir
# Usualmente se definen niveles de significancia estandar: 0.1 0.05, 0.01
# si PValue < significancia, se rechaza H_nula

test <- t.test(x=g$Capital, alternative = 'greater', mu=hip)
test$p.value




plot(g$Capital, type = "l", main="Caminata Aleatoria")
acf(g$Capital, main="Correlograma de Capital")
plot(diff(g$Capital))
acf(diff(g$Capital), main="Detectar Model AR(p)")
pacf(diff(g$Capital), main="Detectar modelo MA(q)")


arima_model <- arima(g$Capital, order = c(2,1,4),
      seas = list(order=c(2,1,4),12))
arima_model$coef

pred <- predict(arima_model,30)$pred
pred
ts.plot(cbind(g$Capital, pred), col = c("blue", "red"), xlab = "")
title(main = "Time Series Prediction ARIMA(2,1,4)",
      xlab = "Time",
      ylab = "Total spending")


match.data.csv <- read.csv("match.data.csv", header = TRUE)
#transformamos en factor el home.team y away.team
match.data.csv$home.team <- factor(match.data.csv$home.team)
match.data.csv$away.team <- factor(match.data.csv$away.team)
match.data.csv.win <- match.data.csv %>% 
  mutate(Ganador = case_when(home.score - away.score > 0 ~ "Local",home.score - away.score < 0 ~ "Visitante",TRUE ~ "Empate"))
library(tidyverse)

datos <- match.data.csv.win %>% filter(home.team == "Real Madrid" | home.team == "Barcelona" | away.team == "Real Madrid" | away.team == "Barcelona" )
rml <- match.data.csv.win %>% filter(home.team == "Real Madrid")
rml
rmv <- match.data.csv.win %>% filter(away.team == "Real Madrid")
rmv
bal <- match.data.csv.win %>% filter(home.team == "Barcelona")
bal
bav <- match.data.csv.win %>% filter(home.team == "Barcelona")
bav


goles_local <- match.data.csv %>% group_by(home.team) %>% summarise(goles_local = sum(home.score), .groups = 'drop')
goles_visitante <- match.data.csv %>% group_by(away.team) %>% summarise(goles_visitante = sum(away.score), .groups = 'drop')

goles_local_encontra <- match.data.csv %>% group_by(home.team) %>% summarise(goles_local_en_contra = sum(away.score), .groups = 'drop')
goles_visitante_encontra <- match.data.csv %>% group_by(away.team) %>% summarise(goles_visitante_en_contra = sum(home.score), .groups = 'drop')
goles_visitante <- select(goles_visitante, -away.team)
goles_local_encontra <- select(goles_local_encontra, -home.team)
goles_visitante_encontra <- select(goles_visitante_encontra, -away.team)
goles <- c()
goles <- cbind(goles_local, goles_visitante,goles_local_encontra,goles_visitante_encontra)
goles$goles_visitante_en_contra
goles <- goles %>% mutate(favor = goles_local / goles_local_en_contra)
goles <- goles %>% mutate(contra = goles_visitante / goles_visitante_en_contra)
goles <- goles %>% mutate(l = (goles_local+goles_visitante))
goles <- goles %>% mutate(v = (goles_local_en_contra+goles_visitante_en_contra))
goles <- goles %>% mutate(diferencia = (goles_local+goles_visitante)/(goles_local_en_contra+goles_visitante_en_contra))
goles <- goles %>% arrange(desc(diferencia))
goles
