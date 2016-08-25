source("Paquetes.R")
source("auxiliar_simulacion.R")
library(shiny)

shinyServer(function(input, output) {
  
  datos <-reactive({
    base<-read.csv(input$mm$datapath)
    names(base)<-tolower(names(base))
    base
  })
  var.partidos <-reactive({
    base<-read.csv(input$var$datapath)
    names(base)<-tolower(names(base))
    base
  })
  var.mm.size <-reactive({
    base<-read.csv(input$mm.size$datapath)
    names(base)<-tolower(names(base))
    base
  })
  
  mapa <- reactive({
    extrae.mapas(input$zip$datapath)
  })
  
  resultados <- eventReactive(input$button1,{
    Base<-genera.base(datos(),input$var.estrato,input$var.ln,input$var.dominio,input$dominio)
    tamanios<-genera.tamanio(var.mm.size())
    muestras(Base,tamanios,as.character(var.partidos()[,input$var.partido]),
             input$show_vars,input$sim,input$var.partido,input$esquema,input$dominio,
             semilla=input$seed,export = TRUE)
  })
  
  output$choose_estrato <- renderUI({
    if (is.null(input$mm))
      return(NULL)
    selectInput("var.estrato", "Variable Estrato",selected = "estrato",
                choices=sort(unique(names(datos()))))
  })
  output$choose_ln <- renderUI({
    if (is.null(input$mm))
      return(NULL)
    selectInput("var.ln", "Variable Tamanio",selected = "ln",
                choices=sort(unique(names(datos()))))
  })
  output$choose_domain <- renderUI({
    if (is.null(input$mm))
      return(NULL)
    selectInput("var.dominio", "Variable Dominio",selected = "dominio",
                choices=sort(unique(names(datos()))))
  })
  output$choose_domain1 <- renderUI({
    if (is.null(input$var.dominio))
      return(NULL)
    selectInput("dominio", "Dominio", 
                choices=sort(c("0 Completo",unique(datos()[,input$var.dominio]))),
                selected = "0 Completo")
  })
  output$choose_partidos <- renderUI({
    if (is.null(input$var))
      return(NULL)
    selectInput("var.partido", "Proceso electoral",
                choices=sort(unique(names(var.partidos()))))
  })
  
  output$choose_partidos1 <- renderUI({
    if (is.null(input$var))
      return(NULL)
    checkboxGroupInput('show_vars', 'Variables a graficar:',
                       as.character(var.partidos()[var.partidos()[,input$var.partido]!="",input$var.partido]))
  })

  output$tabpartido <- renderTable({
    if (is.null(input$var))
      return(NULL)
    data.frame(Partidos=var.partidos()[,input$var.partido])
  })
  
  output$mm.size.table <- renderTable({
    if (is.null(input$mm.size))
      return(NULL)
    as.data.frame(var.mm.size())
  })
  
  output$mapa <- renderPlot({
    if (is.null(input$mm.size))
      return(NULL)
    # Base<-genera.base(datos(),input$var.estrato,input$var.ln,input$var.dominio,input$dominio)
    # tamanios<-genera.tamanio(var.mm.size())
    # muestras(Base,tamanios,as.character(var.partidos()[,input$var.partido]),
    #          input$show_vars,input$sim,input$var.partido,input$esquema,input$dominio,
    #          export = FALSE)["intervalos"]
    resultados()["intervalos"]
  })
  
  output$mytable1 <- DT::renderDataTable({
    # if (is.null(input$mm.size))
    #   return(NULL)
    DT::datatable(as.data.frame(resultados()["resultados"]), options = list(orderClasses = TRUE))
  })
  
  output$print <- renderPrint({
    Base<-genera.base(datos(),input$var.estrato,input$var.ln,input$var.dominio,input$dominio)
    print(resultados()["resultados"])
  })
  
})
