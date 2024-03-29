###actualizacion foto de perfil
#
# This is a Shiny web application. 
#
# Librerias--------------------------------------------------------------
library(shiny)
library(shinythemes)
library(mongolite)
library(rtweet)
library(twitteR)
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
library(writexl)
library(wordcloud2)
library(emojifont)
library(dygraphs)
library(xts)
require(visNetwork)

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


name_columns <- mongo(collection = "name_columns", # Data Table
                      db = "configuration_db", # DataBase
                      url = url_path, 
                      verbose = TRUE)
name_columns <- name_columns$find(query = '{}')
name_columns <- pivot_wider(name_columns, names_from = columns, values_from = columns) 
name_columns <- name_columns[0, c(1:90)]

lista_miembros <- lists_members(list_id = "1359161704270221317", owner_user = "juanibelbis", token = token)

lista_nueva <- c("fpensar" ,"inst_PATRIAar","UniCiudadanaAR","CTAok","FtePatriaGrande","AgenciaAfi","SaludBAP","CostaAugusto9","ManesF","spitta1969",
                 "JuanGrabois","RobiBaradel","PartidoObrero","FdeIzquierda","Miguel_Boggiano", "jorgemacri",  "UNRepublicana" ,  "AvanzaLibertad_", "AgustinLaje","JoRepublicanos",
                 "AmelieGranata", "Fundacionalem")
data_nueva <- name_columns
for(i in lista_politicxs$screen_name){
  if(i %in% lista_nueva){
    df_timeline <- get_timeline(i, n = 1, token = token, retryonratelimit = T)
    df_timeline <- data.frame(lapply(df_timeline, as.character), stringsAsFactors=FALSE)
    data_nueva <- rbind(data_nueva, df_timeline)
  }
  else{}
}
names(lista_miembros)[11] <- "account_created_at"

# HAY QUE UNIR data_nueva con lista_nueva
colnames(lista_miembros)

lista_miembros <- lista_miembros %>% select( user_id, description, profile_image_url, name, followers_count)
data_nueva <- data_nueva %>% select( user_id, description, profile_image_url, name, followers_count)
data_nueva <- rbind(lista_miembros, data_nueva)
data_nueva <- sqldf("select * from data_nueva group by user_id")

### FOTO
data_nueva$img_beg <- '<img src="'
data_nueva$img_finish <- '" height="52"></img>'

data_nueva$image <- paste(data_nueva$img_beg,
                          data_nueva$profile_image_url,
                          data_nueva$img_finish)

data_nueva <- data_nueva %>%
  select(-img_beg, -img_finish)

lista_politicxs <- lista_politicxs %>% select(-description, -name, -profile_image_url, -image, -followers_count)
lista_politicxs <- lista_politicxs %>% left_join(data_nueva, by = "user_id")
lista_politicxs <- lista_politicxs %>% select(-Nombre)
my_query <- mongo(collection = "lista_politicxs", # Data Table
                  db = "configuration_db", # DataBase
                  url = url_path, 
                  verbose = TRUE)
my_query$drop()

my_query$insert(data_politicxs)
