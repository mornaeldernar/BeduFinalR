# Cargamos librerias

library(dplyr)
library(reshape2)
library(ggplot2)

# guardamos los archivos

url1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
url1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
setwd('~/Bedu/R/Sesion 3/Data')
getwd()
download.file(url=url1718,destfile = 'D1-1718.csv',mode='wb')
download.file(url=url1819,destfile = 'D1-1819.csv',mode='wb')
download.file(url=url1920,destfile = 'D1-1920.csv',mode='wb')
#cargamos los archivos

lista <- lapply(dir(), read.csv)


#vemos los datos que tenemos
str(lista)
head(lista)
View(lista)
summary(lista)
#seleccionamos las columnas que queremos
lista <- lapply(lista,select,c(Date,HomeTeam:FTR))

?as.Date
?mutate
# transformamos la columan de fecha en tipo fecha
lista[1] <- lapply(lista[1], mutate, Date = as.Date(Date,"%d/%m/%y"))
lista[2] <- lapply(lista[2], mutate, Date = as.Date(Date,"%d/%m/%Y"))
lista[3] <- lapply(lista[3], mutate, Date = as.Date(Date,"%d/%m/%Y"))


#integramos las tres listas en una sola
datos <- do.call(rbind,lista)
#analizamos los contenidos de datos
dim(datos)
str(datos)
tail(datos)
View(datos)
summary(datos)


conjunta <- data.frame(local=datos$FTHG, visitante=datos$FTAG)
conjunta

frecuencia.local <- table(datos$FTHG)
frecuencia.visitante <- table(datos$FTAG)
frecuencia.conjunta <- table(conjunta)
frecuencia.conjunta
marginal.local <- prop.table(frecuencia.local)
marginal.visitante <- prop.table(frecuencia.visitante)
marginal.conjunta <- data.frame(rbind(prop.table(frecuencia.conjunta)))

marginal.conjunta

library(ggplot2)

barplot(marginal.local, main="Probabilidad de anotar goles como local")
barplot(marginal.visitante, main="Probabilidad de anotar goles como visitante")
heatmap(frecuencia.conjunta,
        Rowv = NA,
        Colv = NA,
        main ="Goles"
        )



# Probabilidades marginales estimadas para los equipos que juegan en casa
(pcasa <- round(table(datos$FTHG)/dim(datos)[1], 3)) 
(pvisita <- round(table(datos$FTAG)/dim(datos)[1], 3)) # Probabilidades marginales estimadas para los equipos que juegan como visitante
(pcta <- round(table(datos$FTHG, datos$FTAG)/dim(datos)[1], 3)) # Probabilidades conjuntas estimadas para los partidos
round(marginal.conjunta,3)

frecuencia.local <- as.data.frame(frecuencia.local)
str(frecuencia.local)
frecuencia.local
frecuencia.local <- frecuencia.local %>% rename(goles = Var1, FRel = Freq)
tail(frecuencia.local)
p <- ggplot(frecuencia.local, aes(x = goles, y = FRel)) + 
  geom_bar (stat="identity", fill = '#FE8A68') +
  ggtitle('Equipo Local') +
  theme_dark()
  
p
frecuencia.visitante <- as.data.frame(frecuencia.visitante)
frecuencia.visitante <- rename(frecuencia.visitante, goles = Var1, FRel = Freq)
tail(frecuencia.visitante)
p <- ggplot(frecuencia.visitante, aes(x = goles, y = FRel)) + 
  geom_bar (stat="identity", fill = '#FE8A68') +
  ggtitle('Equipo visitante')
p
pcta <- melt(pcta) # Función del paquete reshape2
pcta <- rename(pcta, gcasa = Var1, gvisita = Var2, ProbEst = value)
pcta %>% ggplot(aes(gcasa, gvisita)) + 
  geom_tile(aes(fill = ProbEst)) + 
  ggtitle('Probabilidades conjuntas estimadas')+
  scale_fill_gradient(low = '#FCE5DE', high = '#FD6A3E') + 
  theme(axis.text.x = element_text(angle = 90,hjust = 0))+
  labs(x = "Goles equipo local",
       y = "Goles equipo visitante"
  )+
  coord_fixed(ratio=1)+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  scale_y_continuous(breaks = seq(0, 10, by = 1))
pcta

