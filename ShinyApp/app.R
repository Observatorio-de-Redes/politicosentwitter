#
# This is a Shiny web application. 
  #
  # Librerias--------------------------------------------------------------
  library(shiny)
  library(shinythemes)
  library(mongolite)
  library(dplyr)
  library(DT)
  library(shinydashboard)
  library(sqldf)
  library(tidyverse)
  library(lubridate)
  library(shinycssloaders)
  library(plotly)
  library(highcharter)
  library(tidyverse)
  library(stringi)
  library(stringr)
  library(tidytext)
  library(RColorBrewer)
  library(emo)
  library(readxl)
  

rm(list=ls()) #borramos del environment todos los elementos

## nos conectamos a mongolite -----------------------------------------
url_path = 'mongodb+srv://xxxxxxxxxx:xxxxxxxxxx@cluster0.xxxxxxxxxx.mongodb.net/admin' #pen,, config
url_path_2 = 'mongodb+srv://xxxxxxxxxx:xxxxxxxxxx@cluster0.xxxxxxxxxx.mongodb.net/test' # hcdn
url_path_3 = 'mongodb+srv://xxxxxxxxxx:xxxxxxxxxx@cluster0.xxxxxxxxxx.mongodb.net/test' # hcsm
url_path_4 = 'mongodb+srv://xxxxxxxxxx:xxxxxxxxxx@cluster0.xxxxxxxxxx.mongodb.net/admin' #otros, prov

# descargamos los datasets necesarios -------------------------------------

## lista_politicxs ------

my_data <- mongo(collection = "lista_politicxs", # Data Table
                 db = "configuration_db", # DataBase
                  url = url_path, 
                  verbose = TRUE)
data_politicxs <- my_data$find(query = '{}')
### data crecimiento ------

data_crec_db <- mongo(collection = "data_crec", # Data Table
                      db = "CREC_db", # DataBase
                      url = url_path, 
                      verbose = TRUE)
data_crec <- data_crec_db$find(query = '{}')

my_query <- mongo(collection = "count_descargas", # Data Table
                  db = "configuration_db", # DataBase
                  url = url_path, 
                  verbose = TRUE)
count <- my_query$find(query = '{}')

# my_query <- mongo(collection = "data_politicos",
#                   db = "configuration_db", 
#                   url = url_path, 
#                   verbose = TRUE)
# data_politicos <- my_query$find(query = '{}')

n <- 1



# value boxes -------------------------------------------------------------
### bibliografia: ttps://www.r-bloggers.com/2018/06/valuebox-without-shinydashboard-2/

valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-3x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 25px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}


# User Interface ----------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("yeti"),
  # Application title
  navbarPage("Observatorio de redes",
             
             # Descargá los tweets -----------------------------------------------------
             
             tabPanel("Descarga los tweets!", 
                      sidebarLayout(
                        sidebarPanel(
                          ### WIDGET 1 --> text
                          radioButtons(inputId='tipo_organismo', 
                                       label=h3('Selección de Categorias'), 
                                       choiceValues = unique(data_politicxs$Tipo_organismo_2), 
                                       choiceNames = c("Tod@s", "Senadores Nacionales",
                                                       "Funcionario/as Provinciales",
                                                       "Diputados/as Nacionales", 
                                                       "Poder Ejecutivo Nacional",
                                                       "Figuras públicas"  
                                                        ),
                                       selected = NULL), ### SELECCIONAR -
                          ### WIDGET 2 --> descargá
                          
                          uiOutput("seleccion_usuario"),
                          br(),
                          div(shiny::HTML("<h3>Descargá la data:</h3> "),
                          downloadButton('downloadcsv',"CSV"), 
                          downloadButton('downloadxlsx',"XLSX" )),
                          
                          helpText("Aclaración: este es un repositorio donde se almacenan tweets de personajes destacados de la politica nacional. 
                          Podes descargar los tweets eligiendo el organismo al que pertenece, el usuario seleccionado y 
                          apretando el botón correspondiente en
                          'Descargá la data' según si se desea un excel o un csv")
                          
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                          dataTableOutput("data_base")
                        )        
                      )
             ),
             
             
             # Visualiza ---------------------------------------------------------------
             
             
             tabPanel("Visualiza!", sidebarLayout(
               sidebarPanel(
                 ### WIDGET 1 --> text
                 
                 
                 ### WIDGET 2 --> descargá
                 

                 radioButtons(inputId='tipo_organismo2', 
                              label=h3('Selección de Organismo'), 
                              choiceValues = unique(data_politicxs$Tipo_organismo_2), 
                              choiceNames = c("Tod@s", "Senadores Nacionales",
                                              "Funcionario/as Provinciales",
                                              "Diputados/as Nacionales", 
                                              "Poder Ejecutivo Nacional",
                                              "Figuras públicas"  
                              ),
                              selected = NULL), ### SELECCIONAR -
                 
                 ### WIDGET 2 --> descargá
                 
                 uiOutput("seleccion_usuario_2")
                         ), 
               mainPanel(

                   fluidRow(id="main-panel_1",
                            
                            valueBox(value = "favs",
                                     subtitle = "promedio de favs",
                                     icon = "heart",
                                     color = alpha(colour = "#FE435C", alpha = 0.5 )),
                   
                            valueBox(value = "rtweet",
                                      subtitle = "promedio de rtweets",
                                      icon = "retweet",
                                      color = alpha(colour = "#31C485", alpha = 0.5 )),
                           
                           valueBox(value = "followers",
                                    subtitle = "cantidad de followers",
                                    icon = "user-friends", 
                                    color = alpha(colour = "#79E7FB", alpha = 0.5 )),
                             ),
                    
                  
             
             # Frontpage - tweet volume plots - start ----------------------------------
             
             
             fluidRow(
               tabBox(
                 width = 12,
                 tabPanel(
                   status = "primary",
                   title = "Cantidad de Interacciones",
                   br(),
                   withSpinner(plotlyOutput("plot_rt_favs_progress", height = "300px"))
                 ),
                 tabPanel(
                   status = "success",
                   title = "Cantidad de seguidores",
                   br(),
                   withSpinner(plotlyOutput("plot_followers", height = "300px"))
                 ), 
                 tabPanel(
                   status = "success_2",
                   title = "Wordcloud",
                   br(),
                   withSpinner(wordcloud2Output("wordcloud", height = "300px")))
                 ))
             )
             )
             ),
            # Frontpage - tweet volume plots - end ------------------------------------
             
  
             
             # about us ----------------------------------------------------------------
             
             
             tabPanel("About us", 
                  #conoce mas
                      fluidRow(
                        shiny::HTML("<br><center> 
                                            <h1>Conocé más sobre el proyecto</h1> 
                                            </center>"),
                        style = "height:150px;"),
                      fluidRow(
                        div(align = "center", height='35', width='35',
                            tags$span(h4("Este proyecto surge como una iniciativa conjunta del Observatorio de Redes y la Fundación Democracia en Red, 
                                         con el apoyo del National Democratic Institute, con el fin de mejorar la calidad democrática, 
                                         a través de la provisión de datos y contenidos publicados por actores de la vida pública, política institucional para que puedan ser analizados."), 
                                     # style = "font-weight:bold", style = "100px", 
                                      height='35', width='35'))),
                      #logos
                  
                  br(), 
                  br(),
                  fluidRow(
                    column(3),
                  
                    
                    # logo oder ----------------------------------------------------------------
                    column(2,
                           div(class="panel panel-default", width = "700px", height = "200 px",
                               div(class="panel-body",  width = "700px", height = "200 px",
                                   align = "center",
                                   div(
                                     tags$img(src = "http://pbs.twimg.com/profile_images/1225853061081387009/Wybte1kn_normal.jpg", 
                                              width = "80px", height = "80px")
                                   ),
                                   div(align="center",
                                     tags$h5("Observatorio de redes"),
                                     br(),
                                     
                                     column(5, tags$a(href='https://twitter.com/O_de_R',
                                            tags$img(src='https://images.vexels.com/media/users/3/137419/isolated/preview/b1a3fab214230557053ed1c4bf17b46c-logotipo-del-icono-de-twitter-by-vexels.png', 
                                                     height='35', width='35'))),
                                     column(5,  tags$a(href="https://medium.com/@O_de_R",
                                                   tags$img(src='https://w7.pngwing.com/pngs/549/715/png-transparent-web-development-logo-website-web-design-symmetry-internet-thumbnail.png', 
                                                            height='35', width='35')))
                                     )
                                   )
                                    )   ) ,
                  # logo demo en red ----------------------------------------------------------------
                  column(2,
                         div(class="panel panel-default", width = "700px", height = "200 px",
                             div(class="panel-body",  width = "700px", height = "200 px",
                                   align = "center",
                                   div(
                                     tags$img(src = "http://pbs.twimg.com/profile_images/706830818270953472/MLo55kE-_normal.jpg", 
                                              width = "80px", height = "80px")
                                   ),
                                   div(align="center",
                                     tags$h5("Democracia en   red"),
                                     br(),
                                     br(),
                                     

                                     column(5,tags$a(href='https://twitter.com/fundacionDER',
                                                  tags$img(src='https://images.vexels.com/media/users/3/137419/isolated/preview/b1a3fab214230557053ed1c4bf17b46c-logotipo-del-icono-de-twitter-by-vexels.png', 
                                                           height='35', width='35'))),
                                     column(5,  tags$a(href="https://democraciaenred.org/",
                                               tags$img(src='https://w7.pngwing.com/pngs/549/715/png-transparent-web-development-logo-website-web-design-symmetry-internet-thumbnail.png', 
                                                                               height='35', width='35')))
                                   )
                               )
                           )
                    ), 
                    
                  # logo ndi ----------------------------------------------------------------
                  
                    column(2,
                           div(class="panel panel-default", width = "700px", height = "200 px",
                               div(class="panel-body",  width = "700px", height = "200 px",
                            align = "center",
                            div(
                              tags$img(src = "http://pbs.twimg.com/profile_images/1338554872493465602/ociLHbh6_normal.jpg", 
                                       width = "80px", height = "80px")
                            ),
                            div(align="center",
                              tags$h5("National Democratic Institute"),
                              br(),
                              column(5,tags$a(href='https://twitter.com/NDI',
                                                              tags$img(src='https://images.vexels.com/media/users/3/137419/isolated/preview/b1a3fab214230557053ed1c4bf17b46c-logotipo-del-icono-de-twitter-by-vexels.png', height='35', width='35'))),
                              column(5, tags$a(href="https://www.ndi.org/",
                                                               tags$img(src='https://w7.pngwing.com/pngs/549/715/png-transparent-web-development-logo-website-web-design-symmetry-internet-thumbnail.png', 
                                                                        height='35', width='35')))
                            )
                        )
                    )
                  )
                  #cierre ----------------------------------------------------------------
                  
                ),
            # sobre oder ----------------------------------------------------------------
            
                 fluidRow(
                        column(3),
                        #column(3,

                                 h4("Este sitio está desarrollado en R y publicado en una Shiny App, los datos están almacenados en MongoDB Cloud. El código del repositorio lo pueden encontrar en",  
                                            a("Github", href = "https://github.com/Observatorio-de-Redes/poltweet"), ". Este trabajo está publicado bajo una licencia Atribución 2.5 Argentina (CC BY 2.5 AR).") 
                                
                               
                      #  )
                      ,
                        column(3)
                      ),
                      
                      fluidRow(
                        column(3),
                       # column(6,
                               shiny::HTML("<br><br><center> <h2>Sobre el Observatorio de Redes</h2> </center><br>"),
                               shiny::HTML("<h4>El Observatorio de Redes es una iniciativa multidisciplinaria de investigación aplicada,
                               con el fin de experimentar 
                                           y desarrollar metodologías innovadoras para la comprensión y el estudio de los
                                           fenómenos públicos a través del análisis de redes.</h4>"), # sobre oder
                               
                               
                       #        ),
                        column(3) ),
                      
                      fluidRow(
                        
                        style = "height:50px;"),
                      
            # juani & G ----------------------------------------------------------------
            
                      fluidRow(align = "center",
                        column(3),
                        
                        # juani ----------------------------------------------------------------
                        column(2, align = "center",
                               div(class="panel panel-default", 
                                   div(class="panel-body",  width = "700px",
                                       align = "center",
                                       div(
                                         tags$img(src = "http://pbs.twimg.com/profile_images/1030022951754653697/8qORqeLb_normal.jpg", 
                                                  width = "50px", height = "50px")
                                       ),
                                       div(
                                         tags$h5("Juani Belbis"),
                                         tags$h6( tags$i("Visionary & Project Lead"))
                                       ),
                                       div(align="center",
                                         column(5, tags$a(href='https://twitter.com/juanibelbis',
                                                                         tags$img(src='https://images.vexels.com/media/users/3/137419/isolated/preview/b1a3fab214230557053ed1c4bf17b46c-logotipo-del-icono-de-twitter-by-vexels.png', height='35', width='35'))),
                                         column(5, tags$a(href="https://github.com/juanibelbis",
                                                                          tags$img(src='https://image.flaticon.com/icons/png/512/25/25231.png', 
                                                                                   height='35', width='35')))
                                       )   )   ) ),
                        # guada ----------------------------------------------------------------
                        column(2,align = "center",
                               div(class="panel panel-default",
                                   div(class="panel-body",  width = "700px", 
                                       align = "center",
                                       div(
                                         tags$img(src = "http://pbs.twimg.com/profile_images/1278038681668988934/dto344JK_normal.jpg", 
                                                  width = "50px", height = "50px")
                                       ),
                                       div(
                                         tags$h5("Guada Gonzalez"),
                                         tags$h6( tags$i("Political Scientist & Data Scientist"))
                                       ),
                                       div(align="center",
                                         column(5, tags$a(href='https://twitter.com/guadag12',
                                                                         tags$img(src='https://images.vexels.com/media/users/3/137419/isolated/preview/b1a3fab214230557053ed1c4bf17b46c-logotipo-del-icono-de-twitter-by-vexels.png', height='35', width='35'))),
                                         column(5,  tags$a(href="https://github.com/Guadag12",
                                                                          tags$img(src='https://image.flaticon.com/icons/png/512/25/25231.png', 
                                                                                   height='35', width='35')))
                                       )
                                   )
                               )
                        ),
                        # finish ----------------------------------------------------------------
                        
                      )
             ) # cierra aboutus
  )
)



# Server ------------------------------------------------------------------


server <- function(input, output) {

# primer tab --------------------------------------------------------------
  df.filt <- reactive({
    df.filt=df[data_politicxs$Tipo_organismo_2==input$user_name,] 
    df.filt
  })
  
  output$seleccion_usuario <- renderUI({
    selectInput(inputId="user_name", h3("Seleccionar el usuario"), 
                choices = unique(data_politicxs[data_politicxs$Tipo_organismo_2 == input$tipo_organismo, 'screen_name']), ### SELECCIONAR database
                selected = "SergioMassa"
    )
  })
  
  
  ## boton de descargq    
  output$downloadcsv <- downloadHandler(
    filename = function(){paste0(input$user_name, "_database.csv")}, 
    content = function(fname){
      my_query <- mongo(collection = paste0(input$user_name),
                        db = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'database']), 
                        url = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'url_path']), 
                        verbose = TRUE)
      database <- my_query$find(query = '{}')   
      database <- database %>% arrange(desc(created_at))
      write.csv(database, fname)
    }
  )
  
  output$downloadxlsx <- downloadHandler(
    filename = function(){paste0(input$user_name, "_database.xlsx")}, 
    content = function(fname){
      my_query <- mongo(collection = paste0(input$user_name),
                        db = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'database']), 
                        url = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'url_path']), 
                        verbose = TRUE)
      database <- my_query$find(query = '{}')   
      database <- database %>% arrange(desc(created_at))
      write.xlsx(database, fname)
    }
  )
  

  ####
  output$data_base <- DT::renderDataTable({
    data_politicxs_table <- data_politicxs %>% 
      filter(data_politicxs$Tipo_organismo_2 == input$tipo_organismo) %>% 
      arrange(desc(as.numeric(followers_count))) %>%
      rename( "Usuario" = screen_name, 
                               "Descripción" = description,
                                "Organismo" = Tipo_organismo,
                                "Imagen" =image, 
                                "Seguidores"=followers_count, 
                                "some_names"=Nombre,
                                "Nombre" =name) %>%
      select('Imagen', Usuario, Nombre, Descripción, 'Seguidores', Organismo) 

    DT::datatable(data_politicxs_table, escape = FALSE) # HERE
  })
  
# segundo / visualizacion tab --------------------------------------------------------------
  
  output$seleccion_usuario_2 <- renderUI({
    selectInput(inputId="user_name_2", h3("Seleccionar el usuario"), 
                choices =unique(data_politicxs[data_politicxs$Tipo_organismo_2 == input$tipo_organismo2, 'screen_name']), ### SELECCIONAR database
                selected = "@SergioMassa"
    )
  })
  
  ### select user 2 --> NO ET ESTA TRAYENDO LA DATA DE ESA EPRSONA
  df.filt2 <- reactive({
    my_query_2 <- mongo(collection = paste0(input$user_name_2),
                      db = paste0(data_politicxs[data_politicxs$screen_name == input$user_name_2, 'database']), 
                      url = paste0(data_politicxs[data_politicxs$screen_name == input$user_name_2, 'url_path']), 
                      verbose = TRUE)
    df.filt2 <- my_query_2$find(query = '{}')   
    #df.filt2 <- data.frame(lapply(df.filt2, as.character), stringsAsFactors=FALSE)
    df.filt2
  })
  
  df.filt3 <- reactive({
    
    data_crec_db_3 <- mongo(collection = "data_crec", # Data Table
                          db = "CREC_db", # DataBase
                          url = url_path, 
                          verbose = TRUE)
    data_crec_3 <- data_crec_db_3$find(query = '{}')
    df.filt3 <- data_crec_3[data_crec_3$screen_name == input$user_name_2, ]
    #df.filt3 <- data_crec_3[data_crec_3$screen_name == "alferdez", ]
    #df.filt3 <- data.frame(lapply(data_crec_3, as.character), stringsAsFactors=FALSE)
    df.filt3
  })
  ### primer granito --> followes

  output$rtweet <- renderText({ 
    df.filt2_rt <- df.filt2()
   round(mean(as.numeric(df.filt2_rt[df.filt2_rt$is_retweet == F, "retweet_count"]), na.rm = T),1)
  })
  
  output$favs<- renderText({ 
    df.filt2_fav <- df.filt2()
    round(mean(as.numeric(df.filt2_fav[df.filt2_fav$is_retweet == F, "favorite_count"]), na.rm = T),1)
    
  })
  
  output$followers<- renderText({ 
    df.filt3_foll <- df.filt3()
    paste0(df.filt3_foll[df.filt3_foll$date == max(df.filt3_foll$date, na.rm=T), "followers_count"])
  })
  
  
  output$plot_rt_favs_progress <- renderPlotly({
    df_rtfav <- df.filt2()  %>%
      filter(is_retweet == F) %>%
      select(created_at, retweet_count, favorite_count) %>%
      mutate(retweet_count = as.numeric(as.character(retweet_count)), 
             favorite_count = as.numeric(as.character(favorite_count) ), 
             created_at = as.Date(created_at)) %>%
    arrange(created_at) 
    
    plot_ly(x = df_rtfav$created_at, y = df_rtfav$favorite_count, name = "Favoritos",
            type="scatter", mode="lines",line = list(color = alpha('#fe435c',alpha=0.5), shape = "spline")) %>%
      add_trace( x = df_rtfav$created_at, y = df_rtfav$retweet_count, name = "Rtweets",
                 type="scatter", mode="lines",line = list(color = alpha('#31C485',alpha=0.5), shape = "spline")) %>%
      layout( title = paste0("Evolución en la cantidad de interacciones de @", input$user_name_2), 
              xaxis = list(title = "Fecha") ,
              yaxis = list(title = "Cantidad"))
    

  })
  
output$plot_followers <- renderPlotly({
  df3 <- df.filt3()  %>%
    filter(date >="2021-01-23" & screen_name == input$user_name_2) %>%
    select(date, followers_count, friends_count  ) %>%
    mutate(friends_count = as.numeric(as.character(friends_count)), 
           followers_count = as.numeric(as.character(followers_count) )) %>%
    arrange(date)
  
    #plot_ly(x = df3$date, y = df3$followers_count, name = "Seguidores",
    #        type="scatter", mode="lines",line = list(color = '#38bff5', shape = "spline")) %>%
    plot_ly( x = df3$date, y = df3$friends_count, name = "Seguidos",
               type="scatter", mode="lines",line = list(color = '#b575ff', shape = "spline")) %>%
    layout( title = paste0("Evolución en la cantidad de Followers y Friends de @", input$user_name_2), 
    xaxis = list(title = "Fecha") ,
    yaxis = list(title = "Cantidad"))
  
  })

  output$wordcloud <- renderWordcloud2({
    
    stop_words_es <- read.table("https://github.com/Alir3z4/stop-words/raw/master/spanish.txt")
    names(stop_words_es)[1] <- "word"
     df.filt2() %>%
      mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
             text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
             text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
      unnest_tokens(word, text, token = "tweets") %>%
      filter(!word %in% stop_words_es$word,
             !word %in% str_remove_all(stop_words_es$word, "'"),
             str_detect(word, "[a-z]"),
             !str_detect(word, "^#"),         
             !str_detect(word, "@\\S+")) %>%
      filter(!word %in% stop_words$word,
             !word %in% str_remove_all(stop_words$word, "'"),
             str_detect(word, "[a-z]"),
             !str_detect(word, "^#"),         
             !str_detect(word, "@\\S+")) %>%
      count(word, sort = TRUE) %>%
       wordcloud2(color = brewer.pal(8, "Dark2")  #size=input$size
               )
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

