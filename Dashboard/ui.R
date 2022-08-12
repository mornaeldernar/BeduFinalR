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
    ),
    
    # Sidebar with a slider input for number of bins
    mainPanel(
      tabsetPanel(
        tabPanel("Graficos de barras", 
                 h3("Graficos de barras"),
        ),
        tabPanel("Imagenes Postwork",
                 img(src = "goles_local.png",height=450,width=450),
                 img(src = "goles_visitante.png",height=450,width=450),
                 img(src = "probabilidad_conjunta.png",height=450,width=450)
        ),
        tabPanel("Data Table",dataTableOutput("data_table")),
        tabPanel("Grafico Factores",
                 h3("Grafico Factores"),
        ),
      )
    )
  )
)
