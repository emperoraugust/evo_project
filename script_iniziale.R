#Uso R studio


if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("geosphere")) install.packages("geosphere")

library(dplyr)
library(ggplot2)
library(reshape2)
library(geosphere)



store_data<-read.csv("Data/store_data.csv")
airport_master<-read.csv("Data/airport_master.csv")
weather_data<-read.csv("Data/weather_data.csv")
store_master<-read.csv("Data/store_master.csv")

n<-nrow(airport_master)
airport_master <- airport_master[1:(n-1),]


colnames(store_data)[1]<-"id_store"
colnames(store_master)[1]<-"id_store"
colnames(weather_data)[1]<-"id_airport"

table(store_data$id_store)


store_data <- arrange(store_data,txn_year)

i<- 1
for(id in store_master$id_store){
    assign(paste("data_store",toString(i),sep=""),
           filter(store_data,id_store==id))
    i<-i+1
}

rm(store_data)  

table(data_store5$txn_week)
table(data_store9$txn_week)
table(data_store8$txn_week)

#Mancano 21 settimane del 2015 nello store 6 dalla 27 alla 47  
#Mancano 3 settimane del 2015 nello store 9 dalla 46 alla 48
#Lo store 8 con l'id 39740 non ha dati disponibili. 
#Riempo i valori mancanti con NA

for(j in 27:47){
    vec<-c(data_store5$id_store[1],2015,j,rep(NA,7))
    data_store5<-rbind(data_store5,vec)
}

for(j in 46:48){
    vec<-c(data_store9$id_store[1],2015,j,rep(NA,7))
    data_store9<-rbind(data_store9,vec)
}

data_store5<-arrange(data_store5,txn_year,txn_week)
data_store9<-arrange(data_store9,txn_year,txn_week)

#Voglio determinare la stazione meteo più vicina per ogni negozio
nearest_airport<-vector('numeric')

for(i in 1:nrow(store_master)){
    x<-cbind(airport_master$lat,airport_master$lon)
    lista<-lapply(seq_len(nrow(x)), function(i) x[i,])
    vect_dist <- sapply(lista,
                       FUN = distm,
                       y = c(store_master$lat[i],store_master$lon[i]),
                      fun = distVincentyEllipsoid)
    dist<-min(vect_dist)     #distanza in metri
    nearest_airport<-c(nearest_airport,airport_master$airport_id[vect_dist==dist])
}

rm(airport_master)

store_master<-cbind(store_master,nearest_airport)
weather_data<-weather_data %>% 
              filter(id_airport %in% nearest_airport) %>%
              select(id_airport,date,max_tempC,
                     mean_tempC,min_tempC,precipitationmm,cloud_cover)
    


