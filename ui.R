library(shiny)
#install.packages("shinythemes")
library(shinythemes)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("shinydashboardPlus")
library(shinydashboardPlus)
library(readr)
library(lubridate)
library(DT)

c2 <- read_csv("c2.csv")
c2$mes <- month(c2$Fecha)


shinyUI(navbarPage(
  #theme = bslib::bs_theme(bootswatch = "darkly"),
  theme = shinytheme("flatly"),
  "Parcial 2",
  
  
# Introduccion ------------------------------------------------------------  
  
  navbarMenu("Bienvenida",
             tabPanel("Bienvenida", 
                      h2(strong("Introduccion")), br(),
                      p("Este trabajo busca demostrar nuestras habilidades en shiny apps."), p("Es un 
                        proyecto que se realizo en conjunto a lo largo de dos semanas, para presentar finalmente esta aplicacion."), 
                      p("El fin de este es demostrar nuestras habilidades utilizando shiny. Shiny es un paquete de R,
                        que permite comunicar al usuario una historia utilizando cierta data en especifico,  
                        permitiendole a este interactuar con el analisis que nosotros hayamos realizado."),
                      br(), h3("Requisitos del trabajo"), br(),
                      p("1. Tablas"),
                      p("2. Graficas"),
                      p("3. Input"),
                      p("4. Layouts"),
                      p("5. Interactividad"),
                      p("6. Reactividad"),
                      p("7. Update Function"),
                      p("8. Parámetros en el URL")
                      ),
             "----",
             tabPanel("Dataset", h1(strong("Dataset")), br(),
                      p("Para este trabajo decidimos utilizar una base de datos llamada que brinda informacion acerca 
                        de una empresa que da mantenimiento a postes electricidad."),
                      br(), h3("Campos") ,p("Estos datos contienen la siguiente informacion:"),br(),
                      p(" -  Fecha, ID (numero del poste)"),
                      p(" -  Transporte (el tipo de vehiculo que realizo el mantenimiento)"),
                      p(" -  Cod (el tipo de servicio que se realizo)"),
                      p(" -  Costo Total (lo que costo realizar el servicio)"),
                      p(" -  Costo directo (relacionado con la gasolina, la distancia, etc.)"),
                      p(" -  Costo Fijo (costo fijo por tipo de servisio)"),
                      p(" -  Origen (La empresa electrica tiene dos centros, el origen marca de cual de los dos partio el vehiculo que realizo el)"),
                      p(" -  Lat (este campo muestra la latitud de la ubicacion en donde se realizo el servicio"),
                      p(" -  Long (este campo muestra la longitud de la ubicacion en donde se realizo el servicio)"),
                      p(" -  height (altura del poste)"),
                      p(" -  Tiempo Recorrido (lo que el vehiculo se tardo en arribar al lugar)")),
             "----",
             tabPanel("Indice", h1(strong("Indice")), br(),
                      p("Este trabajo se divide en las siguientes areas:"), br(),
                      h3("Datos"),p("En esta pagina se presentan los datos con los que se trabajo en una tabla. El fin
                                    es que el usuario tenga un espacio en el que pueda revisar con que informacion se
                                    esta realizando el analisis en esta aplicacion."), 
                      h3("Distribucion de facturacion"), 
                      p("En este panel el usuario podra ver graficamente el comportamiento de lo que la empresa electrica
                        facturo. Se puede interactuar con esta visualizacion, de forma que el usuario pueda agrupar la informacion no
                        solo por mes, sino que tambien por el tipo de transporte que realizo el servicio. Adicionalmente, se puede
                        modificar el color de la grafica."),
                      p(strong("Parametros URL:")),
                      p("Esta visualizacion incluye ademas, la opcion de modificar los distintos parametros de la visualizacion 
                        desde la URL. Las instrucciones para modificar estos parametros se encuentran en el panel respectivo."),
                      h3("Ingresos de Q a $"),
                      p("Muchas veces es necesario poder hacer una conversion segun el tipo de moneda. Este panel permite precisamente 
                        esto. Aqui el usuario podra revisar cuales fueron los ingresos de la empresa seguin el mes en el que opero,
                        y podra saber el monto total segun su moneda de preferencia, ya sea seleccionando quetzales o dolares."), 
                      h3("Filtro de factura"),
                      p("Ahora bien, en los paneles anteriores los datos se presentan a grandes razgos como un resumen. En este espacio
                        el usuario podra ver detalladamente cada factura, filtrando por el monto minimo y maximo de facturacion. Ademas,
                        en el caso que ya no quiera ver los datos segun sus propios filtros, siempre podra regresar a los rangos iniciales")
                      ,br()), 
             "----",
             tabPanel("Integrantes del grupo", h1(strong("Integrantes del grupo")), br(),
                      p("Daniela Dominguez     -   20180365"),
                      p("Luis Pedro Zenteno    -   20190516"),
                      p("Luis Javier Samayoa   -   20190613"),
                      br(),
                      uiOutput("image1", click = "MyImage"), br(), br())
                      ),
              
  

# Mostrar datos -----------------------------------------------------------
    
    navbarMenu("Trabajo",
               
               
               tabPanel("Datos",
                        h1("Presentacion de Datos"),br(),
                        dataTableOutput('tabla')),
               
# Distribucion facturacion / URL ------------------------------------------  

              "----",
               tabPanel("Distribucion de facturacion (+URL)",
                        h1("Distribucion de facturacion"),br(),
                        sidebarLayout(
                          sidebarPanel(selectInput('mes','mes',
                                                   choices = c('Ene','Feb','Mar','Abr','May','Jun',
                                                               'Jul','Ago','Sep','Oct','Nov','Dic'),selected = "Jul"),
                                       radioButtons('transporte','transporte',
                                                    choices = c('Pickup','Camion','Moto'),selected = "Moto"),
                                       selectInput('color','color',
                                                   choices = c('dark green','brown','light blue','grey','orange','purple')),
                                       textInput("url","Copiar URL: ",value = "")),
                          mainPanel(plotOutput('grafica1'))
                        ),
                        h3("Instrucciones parametros URL"),
                        p("Para cambiar los filtros por medio de la URL, se deben seguir los siguientes pasos:
                          En el recuadro que dice ''Copiar URL:'' se crea automaticamente un url, este se debe utilizar como base.
                          Un ejemplo de esta url es la siguiente:"),
                        p("http://127.0.0.1:4551/?mes=Jul&color=dark green&transporte=Moto"),
                        p("Mes del anio: mes = en este se puede modificar el mes de operaciones que se desee seleccionar. Se debe
                          colocar las primeras tres letras del mes en espaniol, con la primera en mayusculas. Ej: Feb (para febrero)."),
                        p("Color del histograma: color = Las posibles opciones son las siguientes: dark green, brown, light blue, 
                          grey, orange, purple."),
                        p("Tipo de vehiculo: transporte = Es la unidad que realizo el servicio. Las opciones son: Pickup, Camion, Moto."), br()
               ),

# Ingresos ----------------------------------------------------------------

              "----",
              tabPanel("Ingresos de Q a $",
                       h1("Ingresos de Q a $"),br(),
                       sidebarLayout(
                         sidebarPanel(selectInput('meses','meses',
                                                  choices = c('Ene','Feb','Mar','Abr','May','Jun',
                                                              'Jul','Ago','Sep','Oct','Nov','Dic'),selected = "Jul"),
                                      selectInput('moneda','moneda',
                                                  choices = c("quetzales",'dolares'))),
                         mainPanel(h1("Ingresos"),
                           h4("Total segun el mes y la moneda seleccionada:"),
                           verbatimTextOutput("total"),
                           br(),
                           br(),
                           h4("Mes seleccionado"),
                           verbatimTextOutput("output_select1"),
                           h4("Moneda seleccionada"),
                           verbatimTextOutput("output_select2")
                         )
                       )
              ),

# Filtro de factura ------------------------------------------------------- 

              "----",                               
              tabPanel("Filtro de factura",
                       h1("Filtro de factura"),br(),
                       p("Se tiene dos opciones para filtrar: por el total de la factura, o por fechas."),
                       tabsetPanel(
                         tabPanel(
                         "Por total factura", br(),
                         numericInput("min1","Limite inferior",
                                      value = 0),
                         numericInput("max1","Limite superior",
                                      value = 500),
                         actionButton('clean1',"Regresar a rango inicial"),
                         hr(),
                         dataTableOutput('tabla1')
                         ) ,
                         tabPanel(
                         "Por fecha", br(),br(), p("Con este filtro puede seleccionar un intervalo en el tiempo, en el que
                                              desea revisar la informacion de la empresa electrica."), 
                         p("Puede seleccionar los datos desde el primero de enero del 2017, hasta el 31 de diciembre del mismo anio."), br(),
                           dateRangeInput("datum",
                                          format = "yyyy-mm-dd",
                                          label = "Seleccione el rango de fechas",
                                          start = "2017-05-01",
                                          end = "2017-05-31",
                                          min = "2017-01-01",
                                          max = "2017-12-31"),
                           actionButton('clean2',"Regresar a rango inicial"),
                           hr(),
                           dataTableOutput('tabla2')
                         )
                       )
                       
                       ))
  )
)

