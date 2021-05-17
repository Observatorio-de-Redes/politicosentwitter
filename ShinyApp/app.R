#
# This is a Shiny web application. 
#
# Librerias--------------------------------------------------------------
library(shiny)
library(shinythemes)
library(mongolite)
library(dplyr)
#library(glue)
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
library(writexl)
library(wordcloud2)
library(emojifont)
library(dygraphs)
library(xts)
require(visNetwork)
require(splitstackshape)

#library(tweetrmd)
#library(httr)
#library(webshot2)
rm(list=ls()) #borramos del environment todos los elementos
options(scipen = 999)

## nos conectamos a mongolite -----------------------------------------
url_path = 'mongodb+srv://new_user_db:password_new@cluster0.gxwrq.mongodb.net/test' #pen,, config
url_path_2 = 'mongodb+srv://new_user_db:password_new@cluster0.1b8mt.mongodb.net/test' # hcdn
url_path_3 = 'mongodb+srv://new_user_db:password_new@cluster0.j59fq.mongodb.net/test' # hcsm
url_path_4 = 'mongodb+srv://new_user_db:password_new@cluster0.mh1ca.mongodb.net/test' #otros, prov
url_path_5 = 'mongodb+srv://new_user_db:password_new@cluster0.bwilj.mongodb.net/test' # data net + data colors

# descargamos los datasets necesarios -------------------------------------

## lista_politicxs ------


my_data <- mongo(collection = "lista_politicxs_shiny", # Data Table
                 db = "configuration_db", # DataBase
                 url = url_path, 
                 verbose = TRUE)
data_politicxs <- my_data$find('{}')
data_politicxs$imagen_circle <- gsub(pattern = "_normal", replacement = "_400x400",data_politicxs$profile_image_url)
data_politicxs2 <- sqldf("SELECT * from data_politicxs GROUP BY user_id")

### data crecimiento ------

data_crec_db <- mongo(collection = "data_crec", # Data Table
                      db = "CREC_db", # DataBase
                      url = url_path, 
                      verbose = TRUE)
data_crec <- data_crec_db$find(query = '{}')

my_data <- mongo(collection = "data_colors", # Data Table
                 db = "data_net", # DataBase
                 url = url_path_5, 
                 verbose = TRUE)
data_colors <- my_data$find('{}')

my_data_base <- mongo(collection = "alferdez", # Data Table
                      db = "PEN_db", # DataBase
                      url = url_path,
                      verbose = TRUE)
data_base <- my_data_base$find(query = '{"status_id":"1337062667576627200"}')
data_base <- data_base[0,]

my_data_2 <- mongo(collection = "data_network_mensual", # Data Table
                   db = "data_net", # DataBase
                   url = url_path_5, 
                   verbose = TRUE)
data_net <- my_data_2$find('{}')
data_net <-data_net %>%
  filter(month_year > "2015-01-01")
data_net$month_year <- as.Date(data_net$month_year)

my_data <- mongo(collection = "data_actualizacion", # Data Table
                 db = "configuration_db", # DataBase
                 url = url_path, 
                 verbose = TRUE)
data_actualizacion <-  my_data$find('{}') 

# modificacion data_actualizacion -------------------------------------------------------------

data_actualizacion_f <- data_actualizacion[data_actualizacion$fecha == max(data_actualizacion$fecha),]
data_actualizacion_f <- cSplit(data_actualizacion_f, 'fecha', sep =" ")
names(data_actualizacion_f)[2] <- "date"
names(data_actualizacion_f)[3] <- "hour"
data_actualizacion_f$fecha <- paste0(data_actualizacion_f$date, " ",data_actualizacion_f$hour)
data_actualizacion_f <- data_actualizacion_f[,-c("date", "hour")]
data_actualizacion_f$fecha <- ymd_hms(data_actualizacion_f$fecha)
# value boxes -------------------------------------------------------------

source("https://raw.githubusercontent.com/Observatorio-de-Redes/politicosentwitter/main/ShinyApp/valueBox_functions2.R")


# User Interface ----------------------------------------------------------

ui <- fluidPage(
  tagList(tags$head(
    tags$script("
      Shiny.addCustomMessageHandler('background-color', function(color) {
        document.body.style.backgroundColor = color;
      });
    ")
  ),
  
  tags$head(tags$style(".navbar{background-color:#c6c6c6;} .navbar{color: #C6C6C6;}")),
  tags$head(HTML("<title> Politic@s en Twitter</title> ")), #Without company logo
  tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/Observatorio-de-Redes/politicosentwitter/main/ShinyApp/www/favicon_1000px.png")),
  
  navbarPage(selected = "hola",  id = "navbarID",
              title = div( img(src='https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/politicos-en-redes.jpg',
                               style="margin-top: -15px; padding-right:10px;padding-bottom:10px", height = 59)),
            
             
             tags$head(tags$style(".navbar{background-color:#c6c6c6;} .navbar{color: #C6C6C6;}")),
             
             # Descargá los tweets -----------------------------------------------------
             tabPanel(value = "hola",
                      title = icon("home"),
                      # Descargá los tweets -----------------------------------------------------
                      fluidRow( 
                        h1(("Politicians on Twitter 🇦🇷"), style="color: white;
                         font-size: 35px; dislay : block; font-weight: bold;"), 
                        h3(("Real time analysis about the performance on Twitter for + than 500 national public servants in Argentina. 
                  We make this repository available to the community with open and reusable data to improve the production of
                  knowledge.")),
                        style="color: white; font-size: 25px;"),
                      br(), br(),
                      
                      column(2,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_fdt.gif",height="30%",width="30%",style="display: block; "))),
                      
                      valueBox_2(value = "cant_tweets",
                                 subtitle = ("tweets processed"),
                                 icon = "database",
                                 color = alpha(colour = "#002687", alpha = 0.5 )),
                      valueBox_2(value = ("cant_users"),
                                 subtitle = "users analyzed",
                                 icon = "user-tie",
                                 color = alpha(colour = "#002687", alpha = 0.5 )),
                      valueBox_3(value = ("ultima_actualizacion"),
                                 subtitle = "last update",
                                 icon = "clock",
                                 color = alpha(colour = "#002687", alpha = 0.5 )),
                      
                      column(3,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_fdt.gif",height="30%",width="30%",style="display: block; "))),
                      column(3,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_cambiemos.gif",height="30%",width="30%",style="display: block; "))),
                      column(3,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_libertarios.gif",height="30%",width="30%",style="display: block; "))),
                      #column(3,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_fdi.gif",height="30%",width="30%",style="display: block; "))),
                      #column(3,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_peronismo.gif",height="30%",width="30%",style="display: block; "))),
                      #column(3,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_fdt.gif",height="30%",width="30%",style="display: block; "))),
                      column(3,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_cambiemos.gif",height="30%",width="30%",style="display: block; "))),
                      column(2,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_libertarios.gif",height="30%",width="30%",style="display: block; "))),
                      
                      div(              br(), br(), br(), br(), br(), br(),
                                        
                                        column(9,h3("This project was developed by", tags$a(href="https://twitter.com/guadag12"," Guadalupe Gonzalez",style = "color: #00e5ff; font-weight: bold;"), 
                                                    "as part of the Political Science research group", tags$a(href="https://twitter.com/O_de_R", "Observatorio de Redes",style = "color: #00e5ff; font-weight: bold;"),
                                                    "at the University of Buenos Aires with the direction of", tags$a(href="https://twitter.com/juanibelbis", "Juani Belbis",style = "color: #00e5ff; font-weight: bold;"),
                                                    "and the support of ",
                                                    tags$a(href="https://twitter.com/fundacionDER", ("Democracia en Red"), style = "color: #00e5ff; font-weight: bold;"),
                                                    "and the financing of the", tags$a(href="https://twitter.com/NDI", "National Democratic Institute",style = "color: #00e5ff; font-weight: bold;"), ".",
                                                    style="color: white; font-size: 20px;")),
                                        column(2,tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_fdt.gif",height="30%",width="30%",style="display: block; ")))),
                      
                      fluidRow(
                        valueBox_oder(),
                        valueBox_der(),
                        valueBox_ndi(),
                        column(2, tags$div(tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img_net/Animation_otrxs.gif",height="30%",width="30%",style="display: block; ")))),

                      
                      tags$hr(),
                      fluidRow(column(2, h5(tags$i(("Created with"), style="color: white;" ,
                                                   tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/RStudio_logo_flat.svg/1280px-RStudio_logo_flat.svg.png", width = "100px"))),
                                     ))),
             tabPanel(title = "Repository", 
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons(inputId='tipo_organismo', 
                                       label= h4('Select the category:'), 
                                       choiceValues = unique(data_politicxs$Tipo_organismo_2), 
                                       choiceNames = c("All","Low Chamber Members", 
                                                       "President, Vicepresident & Ministers",
                                                       "Other Public Figures", 
                                                       "Subnational Servants",
                                                       "High Chamber Members"
                                       ),
                                       selected = NULL), 
                          
                          uiOutput("seleccion_usuario"),
                          br(),
                          div(shiny::HTML("<h4>Download the data*:</h4> "),
                              downloadButton('downloadcsv',"CSV"), 
                              downloadButton('downloadxlsx',"XLSX" )),
                          a("*Metadata of csvs",target="_blank",href="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/descarga_timeline_usuarios.pdf"),
                          
                          br(), 
                          br(),
                          
                          div(shiny::HTML("<h4>Download the data by category**:</h4> "),
                              withSpinner(downloadButton('downloadcsv_all',"CSV"))),
                          helpText('**The estimation time for the download of each category is between 5 and 15 minutes. It’s highly recommend don’t use the “All” category because it will take much time.'),
                          
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                          dataTableOutput("data_base_OUT")
                        )
                        
                      ),
                      tags$hr(),
                      
                      fluidRow(column(2, h5(tags$i("Created with"),
                                            tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/RStudio_logo_flat.svg/1280px-RStudio_logo_flat.svg.png", width = "100px"))),
                               
                               helpText("Atribución 2.5 Argentina (CC BY 2.5 AR)", align = "center",
                                        style = "
                                  color: #868786;
                                  height:30px;  
                                  padding: 10px; ")
                      )
             ),
             
             
             # Visualiza ---------------------------------------------------------------
             
             tabPanel("Visualization",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId='tipo_organismo2', 
                                       label=h4('Select the category:'), 
                                       choiceValues = unique(data_politicxs$Tipo_organismo_2), 
                                       choiceNames = c("All","Low Chamber Members", 
                                                       "President, Vicepresident & Ministers",
                                                       "Other Public Figures", 
                                                       "Subnational Servants",
                                                       "High Chamber Members"
                                       ),
                                       selected = NULL), 
                          
                          uiOutput("seleccion_usuario_2"), 
                          
                        ), 
                        mainPanel(
                          
                          fluidRow(
                            column(3, uiOutput("imagen_circulo")),
                            htmlOutput("nombre"),
                            #  htmlOutput("account_created"),
                            htmlOutput("description"),
                            br(),
                            
                            valueBox(value = "favs",
                                     subtitle = "mean of favs",
                                     icon = "heart",
                                     color = alpha(colour = "#FE435C", alpha = 0.5 )),
                            
                            valueBox(value = "rtweet",
                                     subtitle = "mean of rtweets",
                                     icon = "retweet",
                                     color = alpha(colour = "#31C485", alpha = 0.5 )),
                            
                            valueBox(value = "followers",
                                     subtitle = "number of followers",
                                     icon = "user-friends", 
                                     color = alpha(colour = "#79E7FB", alpha = 0.5))),
                          
                          br(),
                          fluidRow(
                            tabBox(
                              width = 12,
                              tabPanel(
                                status = "primary",
                                title = "Interactions",
                                br(),
                                withSpinner(dygraphOutput("plot_rt_favs_progress", height='400px',width='100%'))
                              ),
                              tabPanel(
                                status = "success",
                                title = "Followers",
                                br(),
                                withSpinner(dygraphOutput("plot_followers", height='400px',width='100%')), 
                                div(align = "right",
                                    shiny::HTML("<h4>Download the data*:</h4> "),
                                    downloadButton('downloadcsv_fr_foll',"CSV"), 
                                    downloadButton('downloadxlsx_fr_foll',"XLSX" ), 
                                    br(),
                                    a(align = "right", 
                                      "*Metadata of csvs",
                                      target="_blank",
                                      href="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/descarga_interacciones_usuaros.pdf")),
                                br(),
                                br(), 
                                div(align = "right",
                                    shiny::HTML("<h4>Download the data for all:</h4> "),
                                    downloadButton('downloadcsv_all2',"CSV"))
                              ), 
                              
                              tabPanel(
                                uiOutput("dateRange"),
                                status = "success_2",
                                title = "Wordcloud",
                                br(),
                                withSpinner(wordcloud2Output("wordcloud", height='400px',width='100%'))),
                              tabPanel(
                                uiOutput("dateRange_emo"),
                                status = "success_2",
                                title = "Emojis",
                                br(),
                                withSpinner(plotlyOutput("emoji",  height='400px',width='100%')))#,
                              
                              # tabPanel(
                              #   status = "success_1",
                              #   title = "Principales Tweets",
                              #   fluidRow(
                              #     # Frontpage - Most XX Tweets - start --------------------------------------
                              #     column(
                              #       width = 8,
                              #       offset = 2,
                              #       class = "col-md-6 col-md-offset-0 col-lg-4",
                              #       class = "text-center",
                              #       tags$h4(HTML(paste0("❤️️"), "Mayor Cantidad de Favs")),
                              #       withSpinner(uiOutput("dash_most_liked"), proxy.height = "200px")
                              #     ),
                              #     column(
                              #       width = 8,
                              #       offset = 2,
                              #       class = "col-md-6 col-md-offset-0 col-lg-4",
                              #       class = "text-center",
                              #       tags$h4(HTML(paste0("🌟"), "Mayor Cantidad RT'S")),
                              #       withSpinner(uiOutput("dash_most_rt"), proxy.height = "200px")
                              #     ),
                              #     column(
                              #       width = 8,
                              #       offset = 2,
                              #       class = "col-md-6 col-md-offset-0 col-lg-4",
                              #       class = "text-center",
                              #       tags$h4(HTML(paste0("🎉"), "Más reciente")),
                              #       withSpinner(uiOutput("dash_most_recent"), proxy.height = "200px")
                              #     )
                              # )
                              #)
                            )
                          )
                        )
                        
                      ),
                      tags$hr(),
                      
                      fluidRow(column(2, h5(tags$i("Created with"),
                                            tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/RStudio_logo_flat.svg/1280px-RStudio_logo_flat.svg.png", width = "100px"))),
                               
                               helpText("Atribución 2.5 Argentina (CC BY 2.5 AR)", align = "center",
                                        style = "
                                  color: #868786;
                                  height:30px;  
                                  padding: 10px; ")
                      )
             ),
             
             
             tabPanel("Networks", 
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("dateRange_net"),
                          br(),
                          checkboxGroupInput("checkGroup", label = h4("Select the category: "), 
                                             choiceValues = unique(data_politicxs$Tipo_organismo_2), 
                                             choiceNames = c("All","Low Chamber Members", 
                                                             "President, Vicepresident & Ministers",
                                                             "Other Public Figures", 
                                                             "Subnational Servants",
                                                             "High Chamber Members"
                                             ),
                                             selected = "TODXS"),
                          br(),
                          actionButton("action", label = "See profile pictures", icon("eye"), 
                                       style="color: #fff; background-color: #4996e3; border-color: #2e6da4", ),
                          
                          actionButton("action2", label = "Stop watching",icon("eye-slash"), 
                                       style="color: #fff; background-color: #C6C6C6; border-color: #2e6da4"),
                          br(),
                          br(),
                          helpText(
                            tags$ul("Important information:",
                                    tags$li("This network shows the interaction of users downloaded in this application during the selected period based on retweets."),
                                    tags$li("The closer to the date the selected period is, the greater the number of connections between users, due to download restrictions from the Twitter API."),
                                    tags$li("The size of the nodes responds to the amount of retweets obtained by the user and the color to the political sector to which they belong."),
                                    tags$li("The update of this cloud is monthly - first of each month - and the observable changes in it are also month by month."),
                                    #  style="text-align:justify;color:black;background-color:#c6c6c6;padding:15px;border-radius:10px"
                            )
                            # style="text-align:justify;color:black;background-color:#c6c6c6;padding:15px;border-radius:10px"
                          )
                          
                        ),
                        mainPanel(
                          fluidRow(column(10, withSpinner(visNetworkOutput("red_rt", height='700px',width='100%'))),
                                   column(2, helpText(paste("Political Parties:")),
                                          helpText(paste("🟡 Cambiemos")),
                                          helpText(paste("🔵 Frente de Todos")),
                                          helpText(paste("🟠 Peronismo")),     
                                          helpText(paste("🟤 Libertarios")),
                                          helpText(paste("🟣 Otr@s")),  
                                          helpText(paste( "🔴 Frente de Izquierda"))))#,
                          #column(10, withSpinner(visNetworkOutput("red_rt2", height='700px',width='100%')))
                        ) 
                        
                        
                      ),
                      tags$hr(),
                      
                      fluidRow(column(2, h5(tags$i("Created with"),
                                            tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/RStudio_logo_flat.svg/1280px-RStudio_logo_flat.svg.png", width = "100px"))),
                               
                               helpText("Atribución 2.5 Argentina (CC BY 2.5 AR)", align = "center",
                                        style = "
                                  color: #868786;
                                  height:30px;  
                                  padding: 10px; ")
                      )
             )#,
             
             
             # about us ----------------------------------------------------------------
             
             
          #    tabPanel("Conocé más", 
          #             includeHTML("https://github.com/OdeRedes/politicosentwitter/raw/main/ShinyApp/www/conoce_mas_html.html"), 
          #             tags$style(
          #               HTML(
          #                 '
          # #view{
          #   text-align:center;
          # }
          # ')
          #               
          #             ),
          #             tags$hr(),
          #             
          #             fluidRow(column(2, h5(tags$i("Created with"),
          #                                   tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/RStudio_logo_flat.svg/1280px-RStudio_logo_flat.svg.png", width = "100px"))),
          #                      
          #                      helpText("Atribución 2.5 Argentina (CC BY 2.5 AR)", align = "center",
          #                               style = "
          #                         color: #868786;
          #                         height:30px;  
          #                         padding: 10px; ")
          #             )
          #    )
  )
)
)




# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  options(scipen = 999)
  
  observeEvent(input$navbarID, {
    if(input$navbarID == "hola"){
      session$sendCustomMessage("background-color", "#266099")
    } else {
      session$sendCustomMessage("background-color", "white")
    }
  })
  
  output$cant_tweets <- renderText({ 
    paste0(formatC(sum(data_actualizacion$total_tweets), format="f", big.mark = ".", digits=0))
    
    
  })
  
  output$cant_users <- renderText({ 
    paste0(formatC(length(unique(data_politicxs$user_id)),format="f", big.mark = ".", digits=0))
  })
  
  output$ultima_actualizacion <- renderText({ 
    paste0(data_actualizacion_f$fecha)
  })
  
  
  # primer tab --------------------------------------------------------------
  df.filt <- reactive({
    df.filt=df[data_politicxs$Tipo_organismo_2==input$user_name,] 
    df.filt
  })
  
  output$seleccion_usuario <- renderUI({
    selectInput(inputId="user_name", h4("Select the user:"), 
                choices = sort(unique(data_politicxs[data_politicxs$Tipo_organismo_2 == input$tipo_organismo, 'screen_name'])), ### SELECCIONAR database
                selected = "SergioMassa"
    )
  })
  
  
  ## boton de descargq    
  output$downloadcsv <- downloadHandler(
    filename = function(){paste0(input$user_name, "_timeline_database.csv")}, 
    content = function(fname){
      my_query <- mongo(collection = paste0(input$user_name),
                        db = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'database']), 
                        url = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'url_path']), 
                        verbose = TRUE)
      database <- my_query$find(query = '{}')   
      database <- database %>% arrange(desc(created_at))
      write.csv(database, fname, row.names = F)
      my_data1 <- mongo(collection = "data_full_csv", # Data Table
                        db = "data_count", # DataBase
                        url = url_path_5,
                        verbose = TRUE)
      l <- my_data1$find('{}')
      l[l$screen_name == input$user_name, "full_csv"] <- l[l$screen_name == input$user_name, "full_csv"] +1
      my_data1$drop()
      my_data1$insert(l)
    }
  )
  
  output$downloadxlsx <- downloadHandler(
    filename = function(){paste0(input$user_name, "_timeline_database.xlsx")}, 
    content = function(fname){
      my_query <- mongo(collection = paste0(input$user_name),
                        db = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'database']), 
                        url = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'url_path']), 
                        verbose = TRUE)
      database <- my_query$find(query = '{}')   
      database <- database %>% arrange(desc(created_at))
      write_xlsx(database, fname)
      
      my_data2 <- mongo(collection = "data_full_excel", # Data Table
                        db = "data_count", # DataBase
                        url = url_path_5,
                        verbose = TRUE)
      l <- my_data2$find('{}')
      l[l$screen_name == input$user_name, "full_excel"] <-  l[l$screen_name == input$user_name, "full_excel"]  +1
      my_data2$drop()
      my_data2$insert(l)
      
    }
  )
  
  output$downloadcsv_all <- downloadHandler(
    filename = function(){paste0(input$tipo_organismo, "_timeline_database.csv")},
    content = function(fname){
      for(i in data_politicxs[data_politicxs$Tipo_organismo_2 == input$tipo_organismo, 'screen_name']){
        my_query <- mongo(collection = i,
                          db = paste0(data_politicxs[data_politicxs$screen_name == i, 'database']),
                          url = paste0(data_politicxs[data_politicxs$screen_name == i, 'url_path']),
                          verbose = TRUE)
        database <- my_query$find(query = '{}')
        data_base <- dplyr::bind_rows(database, data_base) 
      }
      write.csv(data_base, fname, row.names = F)
      
    }
  )
  
  output$data_base_OUT <- DT::renderDataTable({
    data_politicxs_table <- data_politicxs %>% 
      filter(data_politicxs$Tipo_organismo_2 == input$tipo_organismo) %>% 
      arrange(desc(as.numeric(followers_count))) %>%
      rename( "User" = screen_name, 
              "Description" = description,
              "Category" = Tipo_organismo,
              "Image" =image, 
              "Followers"=followers_count, 
              "Name" =name) %>%
      select('Image', User, Name, Description, 'Followers') 
    
    DT::datatable(data_politicxs_table, escape = FALSE)
  })
  
  # segundo / visualizacion tab --------------------------------------------------------------
  
  output$seleccion_usuario_2 <- renderUI({
    selectInput(inputId="user_name_2", h4("Select the user:"), 
                choices = sort(unique(data_politicxs[data_politicxs$Tipo_organismo_2 == input$tipo_organismo2, 'screen_name'])), ### SELECCIONAR database
                selected = "SergioMassa"
    )
  })
  
  output$imagen_circulo<- renderUI({
    img(src=paste0(data_politicxs[data_politicxs$screen_name==input$user_name_2 & data_politicxs$Tipo_organismo_2== "TODXS",
                                  "imagen_circle"]),
        style= "background-image: url('url');
                                           width:150px;
                                           height:150px;
                                           background-size:contain;
                                           background-position:center;
                                           border-radius:50%;")
    
    
  })
  
  ### select user 2 2021-02-18
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
    
    # data_crec_db_3 <- mongo(collection = "data_crec", # Data Table
    #                         db = "CREC_db", # DataBase
    #                         url = url_path, 
    #                         verbose = TRUE)
    # data_crec_3 <- data_crec_db_3$find(query = '{}')
    df.filt3 <- data_crec[data_crec$screen_name == input$user_name_2,]
    df.filt3
  })
  
  output$downloadcsv_all2 <- downloadHandler(
    filename = function(){paste0( "todxs_interacciones_database.csv")},
    content = function(fname){
      write.csv(data_crec, fname, row.names = F)}
  )
  
  output$nombre <- renderUI({
    nombre_p <- df.filt3()[df.filt3()$date== max(df.filt3()$date),]
    nombre_p[nombre_p$verified == "TRUE", "name"] <- paste0(nombre_p$name, "☑️")
    nombre_p[nombre_p$verified == "FALSE","name" ]<-paste0(nombre_p$name)
    HTML(paste0(h2(as.character(nombre_p$name))))
  })
  # output$account_created <- renderUI({
  #   account_p <- paste(df.filt3()[df.filt3()$date=="2021-01-06","account_created_at"])
  #   account_p <- paste(h5(strong('Cuenta creada el:')),dym(account_p)) 
  #   HTML(account_p)
  # })
  
  output$description <- renderUI({
    description_p <- paste0(
      #h5(strong('Descripción:')), 
      h5(df.filt3()[df.filt3()$date== max(df.filt3()$date),"description"]))
    HTML(description_p)
  })
  
  
  
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
    paste0(df.filt3_foll[df.filt3_foll$date == max(as.Date(df.filt3_foll$date), na.rm=T), "followers_count"])
  })
  
  
  output$plot_rt_favs_progress <- renderDygraph({
    
    df_rtfav <- df.filt2()  %>%
      filter(is_retweet == F) %>%
      select(created_at, retweet_count, favorite_count) %>%
      mutate(Retweets = as.numeric(as.character(retweet_count)), 
             Favoritos = as.numeric(as.character(favorite_count)), 
             time =as.POSIXct(as.character(created_at, "%Y-%m-%d %H:%M:S"))) %>%
      # filter(between(time , input$date[1], input$date[2])) %>%
      select(time, Favoritos, Retweets) %>%
      arrange(time) 
    don=xts( x=df_rtfav[,-1], order.by=df_rtfav$time)
    dygraph(don,  main = paste0("Evolution in the number of Interactions of @",input$user_name_2 )) %>%
      dyRangeSelector() %>%
      dyOptions(maxNumberWidth = 999) %>%
      dySeries("Favoritos",  color = alpha(colour = "#FE435C", alpha = 0.5 )) %>%
      dySeries("Retweets", color = alpha(colour = "#31C485", alpha = 0.5 ))
  })
  
  
  
  output$plot_followers <- renderDygraph({
    
    df3 <- df.filt3()  %>%
      filter(date >="2021-02-11" & screen_name == input$user_name_2) %>%
      select(date, followers_count) %>%
      mutate(Followers = as.numeric(as.character(followers_count)), 
             time = as.Date(as.character(date, "%Y-%m-%d")) ) %>%
      select(time, Followers) %>%
      arrange(time)
    
    don_3=xts( x=df3[,-1], order.by=df3$time)
    dygraph(don_3,  main = paste0("Evolution in the number of Followers of @", input$user_name_2)) %>%
      dySeries("V1", label ="Followers" , color = "#62c6d9", fillGraph = T) %>%
      dyOptions(maxNumberWidth = 999) %>%
      dyRangeSelector() 
    
    
    
  })
  
  output$downloadcsv_fr_foll <- downloadHandler(
    filename = function(){paste0(input$user_name_2, "_interacciones_database.csv")}, 
    content = function(fname){
      
      database <- df.filt3() %>% 
        filter(date >="2021-02-11" & screen_name == input$user_name_2)  %>% 
        arrange(desc(date))
      write.csv(database, fname)
      my_data4 <- mongo(collection = "data_interacions_csv", # Data Table
                        db = "data_count", # DataBase
                        url = url_path_5,
                        verbose = TRUE)
      l <- my_data4$find('{}')
      l[l$screen_name == input$user_name, "interacions_csv"] <- l[l$screen_name == input$user_name, "interacions_csv"] +1
      my_data4$drop()
      my_data4$insert(l)
    }
  )
  
  output$downloadxlsx_fr_foll <- downloadHandler(
    filename = function(){paste0(input$user_name, "_interacciones_database.xlsx")}, 
    content = function(fname){
      database <- df.filt3() %>% 
        filter(date >="2021-02-11" & screen_name == input$user_name_2)  %>% 
        arrange(desc(date))
      write_xlsx(database, fname)
      my_data3 <- mongo(collection = "data_interacions_excel", # Data Table
                        db = "data_count", # DataBase
                        url = url_path_5,
                        verbose = TRUE)
      l <- my_data3$find('{}')
      l[l$screen_name == input$user_name, "interacions_excel"] <-l[l$screen_name == input$user_name, "interacions_excel"] +  1
      my_data3$drop()
      my_data3$insert(l)
    }
  )
  
  
  output$dateRange <- renderUI(
    dateRangeInput('dateRange2',
                   label = shiny::HTML('<h4> Select the time period: </h4> '),
                   start = min(as.Date(df.filt2()$created_at), na.rm = T), end = max(as.Date(df.filt2()$created_at), na.rm = T),
                   min = min(as.Date(df.filt2()$created_at), na.rm = T), max = max(as.Date(df.filt2()$created_at), na.rm = T),
                   separator = " - ", format = "yyyy-mm-dd",
                   startview = 'year', language = 'es', weekstart = 1
    )
  )
  
  
  
  df.filt4 <- reactive({
    wcloud <- df.filt2()
    wcloud$created_at <- as.Date(wcloud$created_at)
    df.filt4 <- wcloud[((as.Date(wcloud$created_at) > as.Date(input$dateRange2[1])) & (as.Date(wcloud$created_at) < as.Date(input$dateRange2[2]))) & (wcloud$is_retweet == F),
                       c("is_retweet", "text")]
    df.filt4$text <- gsub("ñ", "n", df.filt4$text)
    df.filt4$text <- gsub("á", "a", df.filt4$text)
    df.filt4$text <- gsub("é", "e", df.filt4$text)
    df.filt4$text <- gsub("í", "i", df.filt4$text)
    df.filt4$text <- gsub("ó", "o", df.filt4$text)
    df.filt4$text <- gsub("ú", "u", df.filt4$text)
    df.filt4
  })
  
  output$wordcloud <- renderWordcloud2({
    
    stop_words_es <- read.table("https://github.com/Alir3z4/stop-words/raw/master/spanish.txt")
    names(stop_words_es)[1] <- "word"
    
    df.filt4() %>%
      mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
             text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
             text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
      unnest_tokens(word, text, token = "tweets") %>%
      filter(!word %in% stop_words_es$word,
             !word %in% str_remove_all(stop_words_es$word, "'"),
             str_detect(word, "[a-z]"),
             !str_detect(word, "^#"),         
             !str_detect(word, "@\\S+"), 
             !nchar(as.character(word)) <= 3) %>%
      # filter(!word %in% stop_words$word,
      #        !word %in% str_remove_all(stop_words$word, "'"),
      #        str_detect(word, "[a-z]"),
      #        !str_detect(word, "^#"),         
      #        !str_detect(word, "@\\S+")) %>%
      count(word, sort = TRUE) %>%
      wordcloud2(color = brewer.pal(14, "Set3"))
  })
  
  
  
  output$dateRange_emo <- renderUI(
    dateRangeInput('dateRange2_emo',
                   label = shiny::HTML('<h4> Select the time period: </h4> '),
                   start = min(as.Date(df.filt2()$created_at), na.rm = T), end = max(as.Date(df.filt2()$created_at), na.rm = T),
                   min = min(as.Date(df.filt2()$created_at), na.rm = T), max = max(as.Date(df.filt2()$created_at), na.rm = T),
                   separator = " - ", format = "yyyy-mm-dd",
                   startview = 'year', language = 'es', weekstart = 1
    )
  )
  
  
  
  df.filt5 <- reactive({
    emojis_filt <- df.filt2()
    emojis_filt$created_at <- as.Date(emojis_filt$created_at)
    
    df.filt5 <- emojis_filt[((as.Date(emojis_filt$created_at) > as.Date(input$dateRange2_emo[1])) & (as.Date(emojis_filt$created_at) < as.Date(input$dateRange2_emo[2]))) &
                              (emojis_filt$is_retweet == F), c("is_retweet", "text")]
    df.filt5
  })
  
  output$emoji <- renderPlotly({
    
    df.emoji <- df.filt5() %>%
      mutate(emoji = ji_extract_all(text)) %>%
      unnest(cols = c(emoji)) %>%
      select(emoji) %>%
      count(emoji, sort = TRUE) %>% 
      arrange(desc(n))%>%
      head(10) 
    
    fig <- plot_ly(df.emoji, x = ~emoji, y = ~n, type = 'bar', 
                   text =  ~emoji, textposition = 'auto', size = 14, 
                   marker = list(color = 'rgb(158,202,225)'))
    fig <- fig %>% layout(xaxis = list(categoryorder = "array", categoryarray = order(df.emoji$n))) 
    fig %>% layout(title = list(title = "Most frequent emojis used by",
                                titlefont = list(size = 28, color = "orange", family = "Arial")),
                   xaxis = list(title = "Emoji"),
                   yaxis = list(title = "Cantidad"))
    
  })
  
  output$dateRange_net <- renderUI(
    dateRangeInput('dateRange3',
                   label = shiny::HTML('<h4> Select the time period: </h4> '),
                   start =  max(data_net$month_year, na.rm = T)-89,  end = max(data_net$month_year, na.rm = T),
                   min = min(data_net$month_year, na.rm = T), 
                   max = max(data_net$month_year, na.rm = T),
                   separator = " - ", format = "yyyy-mm-dd",
                   startview = 'year', language = 'es', weekstart = 1)
  )
  
  
  data_net_reactive <- reactive({
    lista <- unique(data_politicxs[data_politicxs$Tipo_organismo_2 %in% input$checkGroup, "user_id"])
    data_net_reactive <- data_net[((data_net$month_year > as.Date(input$dateRange3[1])) & 
                                     (data_net$month_year < as.Date(input$dateRange3[2]))) & 
                                    ((data_net$user_id %in% lista) &  (data_net$retweet_user_id %in% lista)), ]
    data_net_reactive
    
  })
  
  output$red_rt <- renderVisNetwork({
    
    data_net_net <- data_net_reactive()
    
    nodes <-  gather(data_net_net, key = "tipo", value = "identificacion", 
                     c("user_id","retweet_user_id"))
    nodes <- sqldf("SELECT *, COUNT(*) as count FROM nodes 
               GROUP BY identificacion HAVING COUNT(*)")
    nodes <- nodes %>% select(identificacion, count)
    nodes <- nodes[!is.na(nodes$identificacion),]
    nodes$id_2 = nodes$identificacion
    nodes <- nodes %>% rename(id = identificacion, label = id_2, value = count)
    data_colors$id <- as.character(as.numeric(data_colors$id))
    # unimos con data colors
    nodes <- left_join( data_colors, nodes, by = "id")  %>% 
      select(id, value, screen_name, color) %>%
      rename(label=screen_name) 
    
    nodes[ is.na(nodes$value ), "value"] <- 0
    
    # armamos los edges -------------------------------------
    data_net_net$value <- as.numeric(as.character(data_net_net$value))
    links <- data_net_net %>% 
      filter(user_id %in% data_colors$id &
               retweet_user_id %in% data_colors$id ) %>%
      group_by(retweet_user_id, user_id) %>% 
      summarise(cantidad = sum(value)) %>%
      rename(from = user_id,
             to = retweet_user_id,
             friendship = cantidad)
    links <- links[!is.na(links$from),]
    links <- links[!is.na(links$friendship),]
    links <- links[!is.na(links$to),]
    
    # VIS NETWORK -------------------------------------
    
    visNetwork(nodes, links, main = list(text = paste0('Interaction on Twitter from the "',day(as.Date(input$dateRange3[1])),'-',month(as.Date(input$dateRange3[1])),'-', year(as.Date(input$dateRange3[1])),
                                                       '" to the "', day(as.Date(input$dateRange3[2])),'-',month(as.Date(input$dateRange3[2])),'-', year(as.Date(input$dateRange3[2])), '"'), 
                                         style = "font-family:Arial;color:#15202b;font-size:20px;text-align:center;")) %>%
      visIgraphLayout() %>%
      visNodes(
        shape = "dot",
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#C4C4C4", highlight = "#C4C4C4"), length = 1000
      ) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1,
                                         hover = T)) %>%
      visPhysics(enabled = F, solver = "repulsion", repulsion = list(nodeDistance = 1000)) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend(position = "right")
    
  })
  
  
  # EVENTO2 -------------------------------------
  
  observeEvent(input$action, {
    
    output$red_rt <- renderVisNetwork({
      
      data_net_net <- data_net_reactive()
      
      nodes <-  gather(data_net_net, key = "tipo", value = "identificacion", 
                       c("user_id","retweet_user_id"))
      nodes <- sqldf("SELECT *, COUNT(*) as count FROM nodes 
               GROUP BY identificacion HAVING COUNT(*)")
      nodes <- nodes %>% select(identificacion, count)
      nodes <- nodes[!is.na(nodes$identificacion),]
      nodes$id_2 = nodes$identificacion
      nodes <- nodes %>% rename(id = identificacion, label = id_2, value = count)
      nodes <- nodes %>% left_join(data_politicxs2  %>% select(id=user_id, screen_name, image=imagen_circle),
                                   by = "id")  %>% 
        select(id, value, screen_name, image#, color         
        ) %>%
        rename(label=screen_name) #%>% 
      #filter(id %in% data_politicxs$user_id)  #%>%
      nodes[ is.na(nodes$value ), "value"] <- 0
      nodes$shape <- "circularImage"
      
      nodes$image <- paste0("https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/img/", nodes$label, ".png")
      # armamos los edges -------------------------------------
      data_net_net$value <- as.numeric(as.character(data_net_net$value))
      
      links <- data_net_net %>% 
        filter(user_id %in% data_colors$id &
                 retweet_user_id %in% data_colors$id ) %>%
        group_by(retweet_user_id, user_id) %>% 
        summarise(cantidad = sum(value)) %>%
        rename(from = user_id,
               to = retweet_user_id,
               friendship = cantidad)
      links <- links[!is.na(links$from),]
      links <- links[!is.na(links$friendship),]
      links <- links[!is.na(links$to),]
      
      #nodes <- nodes %>% drop_na(image)
      visNetwork(nodes, links, main = list(text = paste0('Interaction on Twitter from the "',day(as.Date(input$dateRange3[1])),'-',month(as.Date(input$dateRange3[1])),'-', year(as.Date(input$dateRange3[1])),
                                                         '" to the "', day(as.Date(input$dateRange3[2])),'-',month(as.Date(input$dateRange3[2])),'-', year(as.Date(input$dateRange3[2])), '"'), 
                                           style = "font-family:Arial;color:#15202b;font-size:20px;text-align:center;")) %>%
        visIgraphLayout() %>%
        visNodes(
          shapeProperties = list(useBorderWithImage = TRUE),
          shadow = list(enabled = TRUE, size = 10),
          size = 10
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#C4C4C4", highlight = "#C4C4C4"), length = 1000
        ) %>%
        visOptions(highlightNearest = list(enabled = T, degree = 1,
                                           hover = T)) %>%
        visPhysics(enabled = F, solver = "repulsion", repulsion = list(nodeDistance = 1000)) %>%
        visInteraction(navigationButtons = TRUE)
    })
  })
  
  observeEvent(input$action2, {
    
    output$red_rt <- renderVisNetwork({
      
      data_net_net <- data_net_reactive()
      
      nodes <-  gather(data_net_net, key = "tipo", value = "identificacion", 
                       c("user_id","retweet_user_id"))
      nodes <- sqldf("SELECT *, COUNT(*) as count FROM nodes 
               GROUP BY identificacion HAVING COUNT(*)")
      nodes <- nodes %>% select(identificacion, count)
      nodes <- nodes[!is.na(nodes$identificacion),]
      nodes$id_2 = nodes$identificacion
      nodes <- nodes %>% rename(id = identificacion, label = id_2, value = count)
      data_colors$id <- as.character(as.numeric(data_colors$id))
      # unimos con data colors
      nodes <- left_join( data_colors, nodes, by = "id")  %>% 
        select(id, value, screen_name, color) %>%
        rename(label=screen_name) 
      # nodes$group <- NA
      # nodes[nodes$color == "#0078d7", "group"] <- "Frente de Todos"
      # nodes[nodes$color == "#fff100", "group"] <- "Cambiemos"
      # nodes[nodes$color == "#8e562e", "group"] <- "Libertario"
      # nodes[nodes$color == "#886ce4", "group"] <- "Otr@s"
      # nodes[nodes$color == "#f7630c", "group"] <- "Peronismo"
      # nodes[nodes$color == "#e81224", "group"] <- "Izquierda"
      # nodes$group <- as.factor(nodes$group)
      
      nodes[ is.na(nodes$value ), "value"] <- 0
      
      # armamos los edges -------------------------------------
      data_net_net$value <- as.numeric(as.character(data_net_net$value))
      links <- data_net_net %>% 
        filter(user_id %in% data_colors$id &
                 retweet_user_id %in% data_colors$id ) %>%
        group_by(retweet_user_id, user_id) %>% 
        summarise(cantidad = sum(value)) %>%
        rename(from = user_id,
               to = retweet_user_id,
               friendship = cantidad)
      links <- links[!is.na(links$from),]
      links <- links[!is.na(links$friendship),]
      links <- links[!is.na(links$to),]
      
      # VIS NETWORK -------------------------------------
      
      visNetwork(nodes, links, main = list(text = paste0('Interaction on Twitter from the "',day(as.Date(input$dateRange3[1])),'-',month(as.Date(input$dateRange3[1])),'-', year(as.Date(input$dateRange3[1])),
                                                         '" to the "', day(as.Date(input$dateRange3[2])),'-',month(as.Date(input$dateRange3[2])),'-', year(as.Date(input$dateRange3[2])), '"'), 
                                           style = "font-family:Arial;color:#15202b;font-size:20px;text-align:center;")) %>%
        visIgraphLayout() %>%
        visNodes(
          shape = "dot",
          shadow = list(enabled = TRUE, size = 10)
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#C4C4C4", highlight = "#C4C4C4"), length = 1000
        ) %>%
        visOptions(highlightNearest = list(enabled = T, degree = 1,
                                           hover = T)) %>%
        visPhysics(enabled = F, solver = "repulsion", repulsion = list(nodeDistance = 1000)) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visLegend(position = "right") 
    })
    
  })
  
  
  
  
  # output$dash_most_liked <- renderUI({
  #   
  #  db <- df.filt2() %>%
  #     filter(is_retweet==FALSE) %>%
  #     arrange(desc(favorite_count)) %>%
  #     slice(1) #%>%
  #     # pmap(get_tweet_blockquote) %>%
  #     # .l[[9]][[1]] %>%
  #     # HTML()
  #   tweet_screenshot(tweet_url(db$screen_name, db$status_id)) %>% HTML()    
  #   
  # })
  # 
  # output$dash_most_rt <- renderUI({
  #   
  #   db <-df.filt2() %>%
  #     filter(is_retweet==FALSE) %>%
  #     arrange(desc(retweet_count)) %>%
  #     slice(1) #%>%
  #     # pmap(get_tweet_blockquote) %>%
  #     # .l[[10]][[1]] %>%
  #     # HTML()
  #   tweet_screenshot(tweet_url(db$screen_name, db$status_id)) %>% HTML()    
  #   
  # })
  # 
  # output$dash_most_recent <- renderUI({
  #   db <- df.filt2() %>%
  #     filter(is_retweet==FALSE) %>%
  #     slice(1)# %>%
  #     # pmap(get_tweet_blockquote) %>%
  #     # .[[1]] %>%
  #     # HTML()
  #     tweet_screenshot(tweet_url(db$screen_name, db$status_id)) %>% HTML()    
  # })
  # 
  # 
}

# Run the application 
shinyApp(ui = ui, server = server)
