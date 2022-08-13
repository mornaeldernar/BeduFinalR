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
          menuItem("Gráfico de factores", tabName = "factores", icon = icon("chart-line"))
        )
      ),
      dashboardBody(
        tabItems(
tabItem(tabName= "dashboard",
        fluidRow(
          titlePanel("Dashboard"),
              box(
                selectInput("x", "Selecciona el eje de las X",      # Se indica que la variable "x" será la de entrada
                            choices = list("home.score","away.score")
                ),
                width = 12,
                status = "warning"
              ),
              box(
                title = "Gráfico",
                width = 12,
                solidHeader = TRUE,
                background = "maroon",
                plotOutput("output_plot")
              ),
            ),
          ),
tabItem(tabName = "postwork",
        fluidRow(
          titlePanel("Probabilidades"),
          box(img(src = "goles_local.png",height=450,width="100%"),status="warning", width=6),
          box(img(src = "goles_visitante.png",height=450,width="100%"),status="primary", width=6),
          box(img(src = "probabilidad_conjunta.png",height=450,width="100%"),status="danger", width=6),
            ),
          ),
tabItem(tabName= "data_table",
        fluidRow(
          titlePanel("Data Table"),
              box(dataTableOutput("data_table"),width=12, status = "info")
            ),
          ),
tabItem(tabName = "factores",
        fluidRow(
          titlePanel("Grafico Factores"),
              box(title=("Factores de ganancia promedio"),
              img(src = "momios_promedio.png",height=300,width="100%"),status="warning"),
              box(title=("Factores de ganancia máximo"),
              img(src = "maximo_momios.png",height=300,width="100%"), status="danger"),
            )
          )
        )
      )
    )
  )
)
