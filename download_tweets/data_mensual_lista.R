library(mongolite)
library(mongolite)
library(lubridate)
library(lubridate)
library(tidyverse)
library(stringr)

url_path = 'mongodb+srv://guadag12:Ppincha777@cluster0.gxwrq.mongodb.net/admin' #pen,, config
url_path_5 = 'mongodb+srv://guadag12:Ppincha777@cluster0.bwilj.mongodb.net/test' # data net + data colors

my_data <- mongo(collection = "lista_politicxs", # Data Table
                 db = "configuration_db", # DataBase
                 url = url_path, 
                 verbose = TRUE)
data_politicxs <- my_data$find(query = '{}')
data_politicxs <- data.frame(lapply(data_politicxs, as.character), stringsAsFactors=FALSE)

my_data_1 <- mongo(collection = "data_network_diario", # Data Table
                 db = "data_net", # DataBase
                 url = url_path_5, 
                 verbose = TRUE)
data_net_diaria <- my_data_1$find('{}')
data_net_diaria <- data.frame(lapply(data_net_diaria, as.character), stringsAsFactors=FALSE)
data_net_diaria$created_at[is.na(data_net_diaria$created_at)] <- paste0(Sys.Date()-9)
unique(data_net_diaria$created_at)

write.csv(data_net_diaria, paste0("data_net_diaria",Sys.Date(),".csv" ), row.names = F)


data_net_mensual <- data_net_diaria  %>% 
   filter(user_id %in% data_politicxs$user_id &
           retweet_user_id %in% data_politicxs$user_id ) %>%
    filter(!is.na(retweet_user_id)) %>%
    mutate(created_at = as.Date(created_at), 
        month_year = ceiling_date(created_at, "month") - days(1)) %>%
    select(month_year, user_id,  retweet_user_id) %>%
    group_by(month_year, user_id, retweet_user_id) %>%
    summarise(value = n()) 

my_data_2 <- mongo(collection = "data_network_mensual", 
                 db = "data_net",
                 url = url_path_5, 
                 verbose = TRUE)
my_data_2$insert(data_net_mensual)

my_data_3 <- mongo(collection = "data_network_diario", 
                 db = "data_net",
                 url = url_path_5, 
                 verbose = TRUE)
my_data_3$drop()
