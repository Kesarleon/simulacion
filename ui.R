

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Simulacion de muestras para Validacion"),
  
  tags$head(tags$style(HTML("
    .shiny-text-output {
                            background-color:#fff;
                            }
                            "))),
  # Copy the line below to make a file upload manager
  sidebarLayout(
    sidebarPanel(
      fluidRow(  
        column(12,
               wellPanel(
                 tabsetPanel(
                   id = 'panelset1',
                   tabPanel('Marco Muestral', 
                            fileInput("mm", label = h3("Marco Muestral"),width = '300px'),
                            fluidRow(
                              column(4,
                                     uiOutput("choose_estrato")
                              ),
                              column(4,
                                     uiOutput("choose_ln")
                              ),
                              column(4,
                                     uiOutput("choose_domain")
                              )
                            )
                   ),
                   tabPanel('Variables partidistas', 
                            fileInput("var", label = h3("Variables partidistas"),width = '300px'),
                            fluidRow(
                              column(6,uiOutput("choose_partidos")),
                              column(6,uiOutput("choose_partidos1"))
                            )
                            
                   ),
                   tabPanel('Mapa', 
                            fileInput("zip", label = h3("Mapa de la Entidad (en .zip)"),width = '300px')
                   )
                 ) 
               )
        )
      ),
      hr(),
      fluidRow(
        column(12,
               h3("Configuracion de la muestra"),
               wellPanel(
                 selectInput("esquema", "Esquema de muestreo", choices=c("PPT Sistematico","Sistematico simple")),
                 uiOutput("choose_domain1"),
                 numericInput("sim", "Numero de simulaciones", 10),
                 numericInput("seed", "Numero de semilla", round(runif(1)*100))
                 # textInput("caption", "Ingrese el nombre del Dominio", "Estado")
               )
        )
      ),
      hr(),
      fluidRow(
        column(12,
               h3("Tamanio de Muestra"),
               wellPanel(
                 fluidRow(
                   column(6,fileInput("mm.size", label = h3("Tamanio de muestra"),width = '300px')),
                   column(6,tableOutput("mm.size.table"))
                 )
               )
        )
      ),
      actionButton("button1","MAGIC")
    ),
    mainPanel(
      wellPanel(
        tabsetPanel(
          id = 'panelset2',
          tabPanel('Intervalos',plotOutput("mapa")),
          tabPanel('Resultados.simulacion',DT::dataTableOutput('mytable1'))
        ) 
      )
      # verbatimTextOutput("print")
    )
  )
  
 


  
))
