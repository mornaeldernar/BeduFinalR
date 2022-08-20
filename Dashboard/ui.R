#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(theme = "mytheme.css",
    dashboardPage(
      dashboardHeader(title =  "BEDU/DS Team 18"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Probabilidades", tabName = "postwork", icon = icon("chart-column")),
          menuItem("Data Table", tabName = "data_table", icon = icon("table")),
          menuItem("Gráfico de factores", tabName = "factores", icon = icon("chart-line")),
          menuItem("Hipotesis", tabName = "hipotesis", icon = icon("header")),
          menuItem("Hallazgos", tabName = "hayazgos", icon = icon("search")),
          menuItem("Equipo", tabName = "equipo", icon = icon("users"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName= "dashboard",
                fluidRow(
                  titlePanel("Goles a favor y en contra por equipo"),
                  
                  box(
                    selectInput("x", "Seleccione los goles",
                                choices = c("local" = "home.score", "visitante" = "away.score")),
                    width = 12,
                    status = "success"
                  ),
                  box(
                    title = "Gráfico",
                    width = 12,
                    status="primary",
                    solidHeader = TRUE,
                    plotOutput("output_plot")
                  ),
                  
                ),
          ),
          tabItem(tabName = "postwork",
                fluidRow(
                  titlePanel("Probabilidades Marginales"),
                  box(
                    solidHeader = TRUE,
                    title="Probabilidad marginal del número de goles que anota el equipo local",
                    img(src = "gol_local.png",height=300,width=300),status="primary", width=6
                  ),
                  box(
                    
                    solidHeader = TRUE,
                    title="Probabilidad marginal del número de goles que anota el equipo visitante",
                    img(src = "goles_visitante_v.png",height=300,width=300),status="primary", width=6),
                  box(
                    solidHeader = TRUE,
                    title="Probabilidad conjunta de los goles que anotan el equipo local y visitante en un partido",
                    img(src = "probabilidad_conjuntas.png",height=300,width=400),status="primary", width=6),
                ),
          ),
          tabItem(tabName= "data_table",
                fluidRow(
                  titlePanel("Data Table de la liga española 2010 - 2020"),
                  solidHeader = TRUE,
                  box(dataTableOutput("data_table"),width=12, status = "info")
                ),
          ),
          tabItem(tabName = "factores",
                fluidRow(
                  titlePanel("Gráfico de factores de ganancia promedio y máximo"),
                  box(title=("Factores de ganancia promedio"),
                      solidHeader = TRUE,
                      img(src = "momios_promedio.png",height=300,width="100%"),status="primary"),
                  box(title=("Factores de ganancia máximo"),
                      solidHeader = TRUE,
                      img(src = "momios_maximo.png",height=300,width="100%"), status="primary"),
                  box(width=12,
                    p("El comportamiento del capital invertido de acuerdo a las variables"),
                    p("Scores predecidos (Predicted Home Score/Predicted Away Score)"),
                    p("Anotados (Home Score/Away Score)"),
                    p("Momios promedio (gráfica izquierda)"),
                    p("Momios máximos (gráfica derecha)"),
                    p("No refleja tendencia alguna, se observa que lo valores no fluctuan de manera uniforme 
al rededor de la media."),
p("Además, se observa que no capta estacionalidad, ya que no se repite periódicamente 
un mismo patrón sistemático."))
                )
          ),

tabItem(tabName = "hipotesis",
        fluidRow(
          titlePanel("Prueba de hipótesis"),
          box(width=12,
              title=("Factores de ganancia máximo"),
              solidHeader = TRUE,
              img(src = "winratecharts.png",height=300,width="100%"), status="primary"),
          box(
            img(src = "codigo.png",height=300,width="100%"), status="primary"),
          box(
            title=("Análisis"),
            solidHeader = T,
            status="primary",
            p("De acuerdo con el análisis se puede afirmar que Jugar como local o visitante tiene influencia sobre la tasa de victoria dado que p-value < Nivel de significancia."),
            p("Por lo tanto se rechaza H0"),
            p("Se puede observar, además, en las gráficas de caja y bigotes, que existe diferencia significativa entre jugar como local o visitante."))
        )
),

tabItem(tabName = "hayazgos",
        fluidRow(
          titlePanel("Hallazgos"),
          box(width=12,
              title=("Rate de victorias como local y como visitante"),
              status="primary",
              solidHeader = TRUE,
              img(src = "winratebarcelona.png",height=300,width="100%")),
          box(width=6,
              title=("Goles por partido"),
              status="primary",
              solidHeader = TRUE,
              img(src = "goleo.png",height=300,width="100%")),
          
          box(
            title = "Distribución de partidos ganados",
            width = 6,
            status="primary",
            solidHeader = TRUE,
            plotOutput("pie_plot")
          ),
          
        )
),
          tabItem(tabName = "equipo",
              fluidRow(
                titlePanel("Equipo"),
                box(title=("Alef Gama"),img(src = "Alef.png",width="100%")),
                box(title=("Arturo Solís"),img(src = "Arturo2.png",width="100%")),
                box(title=("Jacob Muñoz"),img(src = "Jacob.png",width="100%")),
                box(title=("Javier Castillo"),img(src = "Javier.png",width="100%")),
                box(title=("Rafael Jiménez"),img(src = "Rafa.png",width="100%"))
              )
          )
        )
      )
    )
  )
)
