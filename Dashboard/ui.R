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
  fluidPage(
    dashboardPage(
      dashboardHeader(title =  "Football"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Probabilidades", tabName = "postwork", icon = icon("chart-column")),
          menuItem("Data Table", tabName = "data_table", icon = icon("table")),
          menuItem("Gráfico de factores", tabName = "factores", icon = icon("chart-line")),
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
                  box(title ="Ganadores como local",
                      fluidRow(
                        column(5),
                        column(2,h3(textOutput("win_local"))),
                        column(5)
                      ),
                      background = "green",
                  ),
                  box(title ="Ganadores como visitante",
                      fluidRow(
                        column(5),
                        column(2,h3(textOutput("win_visitante"))),
                        column(5)
                      ),
                      background = "green",
                  ),
                  box(title ="Empates",
                      fluidRow(
                        column(5),
                        column(2,h3(textOutput("win_empate"))),
                        column(5)
                      ),
                      background = "green",
                  ),
                  box(
                    title = "Gráfico",
                    width = 12,
                    solidHeader = TRUE,
                    background = "green",
                    plotOutput("output_plot")
                  ),
                ),
          ),
          tabItem(tabName = "postwork",
                fluidRow(
                  titlePanel("Probabilidades Marginales"),
                  box(
                    title="Probabilidad marginal del número de goles que anota el equipo local",
                    img(src = "gol_local.png",height=300,width=300),status="warning", width=6
                  ),
                  box(
                    
                    title="Probabilidad marginal del número de goles que anota el equipo visitante",
                    img(src = "goles_visitante_v.png",height=300,width=300),status="primary", width=6),
                  box(
                    title="Probabilidad conjunta de los goles que anotan el equipo local y visitante en un partido",
                    img(src = "probabilidad_conjuntas.png",height=300,width=400),status="danger", width=6),
                ),
          ),
          tabItem(tabName= "data_table",
                fluidRow(
                  titlePanel("Data Table de la liga española 2010 - 2020"),
                  box(dataTableOutput("data_table"),width=12, status = "info")
                ),
          ),
          tabItem(tabName = "factores",
                fluidRow(
                  titlePanel("Gráfico de factores de ganancia promedio y máximo"),
                  box(title=("Factores de ganancia promedio"),
                      img(src = "momios_promedio.png",height=300,width="100%"),status="warning"),
                  box(title=("Factores de ganancia máximo"),
                      img(src = "momios_maximo.png",height=300,width="100%"), status="danger"),
                )
          ),
          tabItem(tabName = "equipo",
              fluidRow(
                titlePanel("Equipo"),
                box(title=("Alef Zain Gama Sandoval")),
                box(title=("Arturo Solís")),
                box(title=("Jacob Muñoz")),
                box(title=("Javier Castillo")),
                box(title=("Rafael Jimenez Orozco"))
              )
          )
        )
      ),
      skin = "green"
    )
  )
)
