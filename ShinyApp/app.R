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

my_query <- mongo(collection = "data_politicos",
                  db = "configuration_db", 
                  url = url_path, 
                  verbose = TRUE)
data_politicos <- my_query$find(query = '{}')


names(data_politicos)[2] <- "Usuario"
names(data_politicos)[3] <- "Descripción"
names(data_politicos)[6] <- "Organismo"
names(data_politicos)[11] <- "Imagen"




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
  navbarPage("Observatorio de Redes",
             
             # Descargá los tweets -----------------------------------------------------
             
             
             tabPanel("Descarga los tweets!", 
                      sidebarLayout(
                        sidebarPanel(
                          ### WIDGET 1 --> text
                          radioButtons(inputId='tipo_organismo', 
                                       label=h3('Selección de Organismo'), 
                                       choiceValues = unique(data_politicxs$Tipo_organismo), 
                                       choiceNames = c("Diputadxs Nacionales", "Funcionarixs Provinciales", "Senadorxs Nacionales",  
                                                       "Otrxs",  "Poder Ejecutivo Nacional" ),
                                       selected = NULL), ### SELECCIONAR -
                          
                          ### WIDGET 2 --> descargá
                          
                          uiOutput("seleccion_usuario"),
                          downloadButton('download',"Download the data")
                          
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
                 radioButtons(inputId='tipo_organismo', 
                              label=h3('Selección de Organismo'), 
                              choiceValues = unique(data_politicxs$Tipo_organismo), 
                              choiceNames = c("Diputadxs Nacionales", "Funcionarixs Provinciales", "Senadorxs Nacionales",  
                                              "Otrxs",  "Poder Ejecutivo Nacional" ),
                              selected = NULL), ### SELECCIONAR -
                 
                 ### WIDGET 2 --> descargá
                 
                 uiOutput("seleccion_usuario_2")
                         ), 
               mainPanel(

                   fluidRow(id="main-panel_1",
                            
                            valueBox(value = "favs",
                                     subtitle = "promedio de favs",
                                     icon = "heart",
                                     color = alpha(colour = "#FE435C", alpha = 0.3 )),
                   
                            valueBox(value = "rtweet",
                                      subtitle = "promedio de rtweets",
                                      icon = "retweet",
                                      color = alpha(colour = "#31C485", alpha = 0.3 )),
                           
                           
                           valueBox(value = "followers",
                                    subtitle = "cantidad de followers",
                                    icon = "user-friends", 
                                    color = alpha(colour = "#79E7FB", alpha = 0.3 )),
                             )
                           )
                    
                 ),
             
             
             # Frontpage - tweet volume plots - start ----------------------------------
             
             
             fluidRow(
               tabBox(
                 width = 12,
                 tabPanel(
                   status = "primary",
                   title = " Progreso en Tweets",
                   withSpinner(plotlyOutput("plot_hourly_tweet_volume", height = "250px"))
                 ),
                 tabPanel(
                   status = "success",
                   title = "Cantidad de tweets por hora",
                   withSpinner(plotlyOutput("plot_tweets_by_hour", height = "250px"))
                 )
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
                        div(align = "center",
                            tags$span(h4("Este proyecto surge como una iniciativa conjunta del Observatorio de Redes y la Fundación Democracia en Red, con el apoyo del National Democratic Institute, con el fin de mejorar la calidad democrática, a través de la provisión de datos y contenidos publicados por actores de la vida pública, política institucional para que puedan ser analizados."), 
                                      style = "font-weight:bold", style = "100px"))),
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
             )
  )
)



# Server ------------------------------------------------------------------


server <- function(input, output) {

# primer tab --------------------------------------------------------------
  df.filt <- reactive({
    df.filt=df[data_politicxs$Tipo_organismo==input$user_name,] 
    df.filt
  })
  
  output$seleccion_usuario <- renderUI({
    selectInput(inputId="user_name", h3("Seleccionar el usuario"), 
                choices = unique(data_politicxs[data_politicxs$Tipo_organismo == input$tipo_organismo, 'screen_name']), ### SELECCIONAR database
                selected = 1
    )
  })
  
  
  ## boton de descargq    
  output$download <- downloadHandler(
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
  
  ####
  output$data_base <- DT::renderDataTable({
    DT::datatable(data_politicos[, c(11, 2, 3, 6)], escape = FALSE) # HERE
  })
  
# segundo / visualizacion tab --------------------------------------------------------------
  
  output$seleccion_usuario_2 <- renderUI({
    selectInput(inputId="user_name_2", h3("Seleccionar el usuario"), 
                choices = unique(data_politicxs[data_politicxs$Tipo_organismo == input$tipo_organismo, 'screen_name']), ### SELECCIONAR database
                selected = 1
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
    #df.filt3 <- data_crec_3[data_crec_3$screen_name == "alferdez", ]
    df.filt3 <- data_crec_3[data_crec_3$screen_name == input$user_name_2, ]
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
  
  
  output$plot_hourly_tweet_volume <- renderPlotly({
    df.filt2() %>%
      tweets_just(created_at, is_topic) %>%
      group_by(is_topic) %>%
      tweets_volume() %>%
      mutate(topic = if_else(is_topic, "topic", "all")) %>%
      ungroup() %>%
      rename(Date = by_time) %>%
      select(-is_topic) %>%
      spread(topic, n, fill = 0) %>%
      plot_ly(x = ~ Date) %>%
      add_lines(y = ~topic, name = TOPIC$name, color = I(ADMINLTE_COLORS$teal)) %>%
      {
        if (!is.null(TOPIC$full_community)) {
          add_lines(., y = ~all, name = TOPIC$full_community, color = I(ADMINLTE_COLORS$purple))
        } else .
      }%>%
      config(displayModeBar = FALSE) %>%
      layout(
        xaxis = list(
          range = c(now(tz_global()) - days(7), now(tz_global())),
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = "Today",
                step = "day",
                stepmode = "todate"),
              list(
                count = 1,
                label = "Yesterday",
                step = "day",
                stepmode = "backward"),
              list(
                count = 7,
                label = "Week",
                step = "day",
                stepmode = "backward"),
              list(step = "all", label = "All"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Tweets"),
        legend = list(orientation = 'h', x = 0.05, y = 0.9),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      ) %>%
      config(collaborate = FALSE, cloud = FALSE, mathjax = NULL)
  })
  
  output$plot_tweets_by_hour <- renderPlotly({
    tweets() %>%
      tweets_just(created_at, is_topic) %>%
      tweets_by_time(by = "1 hour") %>%
      mutate(hour = hour(by_time)) %>%
      group_by(hour, is_topic) %>%
      count() %>%
      ungroup() %>%
      mutate(topic = if_else(is_topic, "topic", "all")) %>%
      select(-is_topic) %>%
      spread(topic, n, fill = 0) %>%
      plot_ly(x = ~hour) %>%
      add_bars(y = ~topic, name = TOPIC$name, color = I(ADMINLTE_COLORS$teal)) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(title = "Tweets"),
        xaxis = list(title = glue::glue("Hour of the Day ({TZ_GLOBAL})")),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
