library(shiny)
library(dplyr)
library(knitr)
library(readr)
library(DT)
library(ggplot2)
library(lubridate)
library(quantmod)

c2 <- read_csv("c2.csv")
c2$mes <- month(c2$Fecha)

shinyServer(function(input, output, session) {
  
  
  output$image1<- renderUI({
    imgurl2 <- "https://images.unsplash.com/photo-1473341304170-971dccb5ac1e?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1770&q=80"
    tags$img(src=imgurl2, width = 800, height = 400)
  })
  
  observeEvent(input$MyImage, { print("Hey there")})
  
  
  output$tabla <- renderDataTable({
    c2 %>% datatable()
  })
  
  
 grafica_mes<-reactive({
    switch(input$mes,
           Ene=hist(c2[which(c2$mes==1 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Enero",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Feb=hist(c2[which(c2$mes==2 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Febrero",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Mar=hist(c2[which(c2$mes==3 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Marzo",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Abr=hist(c2[which(c2$mes==4 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Abril",xlab="Facturación",ylab="Frecuencia", col = input$color),
           May=hist(c2[which(c2$mes==5 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Mayo",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Jun=hist(c2[which(c2$mes==6 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Junio",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Jul=hist(c2[which(c2$mes==7 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Julio",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Ago=hist(c2[which(c2$mes==8 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Agosto",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Sep=hist(c2[which(c2$mes==9 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Septiembre",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Oct=hist(c2[which(c2$mes==10 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Octubre",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Nov=hist(c2[which(c2$mes==11 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Noviembre",xlab="Facturación",ylab="Frecuencia", col = input$color),
           Dic=hist(c2[which(c2$mes==12 & c2$Transporte==input$transporte),]$factura,main = "Histograma de Ingresos de Diciembre",xlab="Facturación",ylab="Frecuencia", col = input$color)
    )
  })
  
  output$grafica1<-renderPlot({
    grafica_mes()
  })
  
  
  ingresos_mes<-reactive({
    switch(input$meses,
           Ene=sum(c2[which(c2$mes==1),]$factura),
           Feb=sum(c2[which(c2$mes==2),]$factura),
           Mar=sum(c2[which(c2$mes==3),]$factura),
           Abr=sum(c2[which(c2$mes==4),]$factura),
           May=sum(c2[which(c2$mes==5),]$factura),
           Jun=sum(c2[which(c2$mes==6),]$factura),
           Jul=sum(c2[which(c2$mes==7),]$factura),
           Ago=sum(c2[which(c2$mes==8),]$factura),
           Sep=sum(c2[which(c2$mes==9),]$factura),
           Oct=sum(c2[which(c2$mes==10),]$factura),
           Nov=sum(c2[which(c2$mes==11),]$factura),
           Dic=sum(c2[which(c2$mes==12),]$factura)
           
           
    )
  })
  
  
  output$output_select1<-renderPrint({
    out<-input$meses
    print(out)
    
  })
  
  
  output$output_select2 <- renderPrint({
    out <- input$moneda
    print(out)
  })
  
  
  output$total <- renderPrint(
    paste(format(
      if (input$moneda == "quetzales") {
        ingresos_mes()
      } else {
        ingresos_mes()/7.8
      } , big.mark = ",",big.interval = 3L, digits=0, scientific=F
    ),input$moneda)
  )
  
  
  output$tabla1 <- renderDataTable({
    c2 %>% 
      filter(factura > input$min1) %>% 
      filter(factura < input$max1) %>% 
      select(Fecha, ID, factura) %>% 
      datatable() 
  })
  
  output$tabla2 <- renderDataTable({
    c2 %>%
      filter(Fecha > input$datum[1]) %>%
      filter(Fecha < input$datum[2]) %>%
      select(Fecha, ID, factura) %>%
      datatable()
  })
  
  
  observeEvent(input$clean1,{
    updateNumericInput(session,'min1', value = 0)
    updateNumericInput(session,'max1', value = 500)
  })
  
  observeEvent(input$clean2,{
    updateDateRangeInput(session,'datum', start = "2017-05-01",
                         end = "2017-05-31")
  })
  
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    mes <- query[["mes"]]
    color_barras <- query[["color"]]
    transporte <- query[["transporte"]]
    if(!is.null(mes)){
      updateSelectInput(session, "mes", selected = mes)
    }
    if(!is.null(color_barras)){
      updateSelectInput(session, "color", selected = color_barras)
    }
    if(!is.null(transporte)){
      updateSelectInput(session, "transporte", selected = transporte)
    }
  })
  
  observe({
    mes<-input$mes
    color<-input$color
    transporte <- input$transporte
    if(session$clientData$url_port==''){
      url1 <- NULL
    } else {
      url1 <- paste0(":",
                     session$clientData$url_port)
    }
    codigo<-paste0("http://",
                   session$clientData$url_hostname,
                   url1,
                   session$clientData$url_pathname,
                   "?","mes=",
                   mes,'&',
                   "color=",
                   color,"&",
                   "transporte=",
                   transporte)
    updateTextInput(session,"url",value = codigo)
  })
  
  # PRUEBA DE LO DEL URL !!!!!!
  
  observe({
    data <- parseQueryString(session$clientData$url_search)
    session$sendCustomMessage(type='updateSelections', data)
    updateSelectInput(session, 'beverage', selected=data$beverage)
  })

})



