# Rattenmonitortool - Draft script

getwd()
library(tidyverse)

# Test visualisatie (Data VMM)

# Read data
VMM<-read.csv('./data/VMM_Data_Test.csv',sep=';')
VMM$Gif<-VMM$Registratie.Hoeveelheid..gr.
VMM$Gif<-as.numeric(VMM$Gif)

# Data wrangling
#Data_Monitor<-aggregate(Gif ~ Gemeente + Maand , data = VMM, sum)
Data_Monitor<-aggregate(Gif ~ Locatie.GPS.Lengte + Locatie.GPS.Breedte + Maand , data = VMM, sum)


Meldingen_Gemeenten<-VMM %>% count(Gemeente, Maand, Locatie.ID)
Meldingen_Gemeenten$n<-rep(1,length.out=length(Meldingen_Gemeenten$n))
Meldingen_Gemeenten_sum<-aggregate(n ~ Gemeente + Maand , data = Meldingen_Gemeenten, sum)

Lon_Gemeente<-aggregate(Locatie.GPS.Lengte ~ Gemeente , data = VMM, mean)
Lat_Gemeente<-aggregate(Locatie.GPS.Breedte ~ Gemeente , data = VMM, mean)
Lon_Lat_Gemeente<-cbind.data.frame(Lon_Gemeente,Lat_Gemeente)

Data_Monitor$Lon<-rep(NA,length.out=nrow(Data_Monitor))
Data_Monitor$Lat<-rep(NA,length.out=nrow(Data_Monitor))
Data_Monitor$Locaties<-rep(NA,length.out=nrow(Data_Monitor))

for (i in 1:nrow(Data_Monitor)) {
  Data_Monitor$Lon[i]<-Lon_Lat_Gemeente$Locatie.GPS.Lengte[which(Lon_Lat_Gemeente$Gemeente ==
                                                                   Data_Monitor$Gemeente[i])]
  Data_Monitor$Lat[i]<-Lon_Lat_Gemeente$Locatie.GPS.Breedte[which(Lon_Lat_Gemeente$Gemeente ==
                                                                   Data_Monitor$Gemeente[i])]
  Data_Monitor$Locaties[i]<-Meldingen_Gemeenten_sum$n[which(Meldingen_Gemeenten_sum$Gemeente ==
                                                              Data_Monitor$Gemeente[i] & 
                                                              Meldingen_Gemeenten_sum$Maand ==
                                                              Data_Monitor$Maand[i])]
}



#read occurence data from gbif
library(rgbif)

name_backbone("Rattus norvegicus")

wkt<- "POLYGON ((5.045752 50.657895, 5.045752 51.318329, 5.925901 51.318329, 5.925901 50.657895, 5.045752 50.657895))"

gbif_download <- occ_download(pred("taxonKey", 2439261),format = "SIMPLE_CSV", pred_gte("year", 2023),pred_within(wkt),
                              user = "michiellathouwers", pwd = "*", email = "michiel.lathouwers@inbo.be") #paswoord invullen

occurence <- occ_download_get('0058520-231002084531237') %>%
  occ_download_import()

occurence<-as.data.frame(occurence)

# Nieuwe visualisatie met heatmap

library(sf)

Provincies<-st_read('./data/Provincies_shapefile/BELGIUM_-_Provinces.shp')
Limburg<-subset(Provincies,Provincies$NAME_2=="Limburg")

occ_sf<-st_as_sf(occurence,coords = c("decimalLongitude", "decimalLatitude"),
         crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )

Data_Monitor$Gif2<-Data_Monitor$Gif/100
Data_Monitor2 <- with(Data_Monitor, Data_Monitor[rep(1:nrow(Data_Monitor), Gif2),])

Data_Monitor2$decimalLongitude<-Data_Monitor2$Locatie.GPS.Lengte
Data_Monitor2$decimalLatitude<-Data_Monitor2$Locatie.GPS.Breedte

ggplot()+geom_sf(data=Limburg)+
  stat_density2d_filled(data=Data_Monitor2,
                        aes(x = decimalLongitude, y = decimalLatitude,
                            alpha=after_stat(level), fill=after_stat(level)),
                        linewidth=2,geom="polygon")+
  geom_hex(occurence, aes(decimalLongitude, decimalLatitude))

# Deploy app (app.R)

library(rsconnect)
library(shiny)

# Run in command line for authorization token
setwd("./app rattus")
gs4_auth(email = "michiel.lathouwers@inbo.be", cache = ".secrets")

#gitignore authorizatioin info
usethis::use_git_ignore(".secrets")
usethis::use_git_ignore("*/.secrets")

## Deploy
setwd("~/GitHub/rattus")
rsconnect::deployApp("./app rattus")




