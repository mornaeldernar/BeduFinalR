#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(
  
  pageWithSidebar(
    
    # Application title
    headerPanel("Dashboard Football"),
    sidebarPanel(
      p("Graficas"),
      selectInput("x", "Selecciona el eje de las X",      # Se indica que la variable "x" será la de entrada
                  choices = colnames(select(match.data.csv,home.score,away.score)),
      )
    ),
    
    # Sidebar with a slider input for number of bins
    mainPanel(
      tabsetPanel(
        tabPanel("Gráficos de barras", 
                 h3(textOutput("output_text")),
                 plotOutput("output_plot"),
        ),
        tabPanel("Imagenes Postwork",
                 img(src = "goles_local.png",height=450,width=450),
                 img(src = "goles_visitante.png",height=450,width=450),
                 img(src = "probabilidad_conjunta.png",height=450,width=450)
        ),
        tabPanel("Data Table",dataTableOutput("data_table")),
        tabPanel("Gráfico Factores",
                 h3("Factores de ganancia promedio"),
                 img(src = "momios_promedio.png",height=300,width="100%"),
                 h3("Factores de ganancia máximo"),
                 img(src = "maximo_momios.png",height=300,width="100%"),
        ),
      )
    )
  )
)
