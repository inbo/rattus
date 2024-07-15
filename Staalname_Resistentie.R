
library(ggplot2)
library(sf)
library(tidyverse)

# overzicht staalname resistentie

shapefiles <- list.files(path = "C:/Users/michiel_lathouwers/Downloads/kaarten VMM shapefiles-20231030T081024Z-001/kaarten VMM shapefiles",
                         pattern = "\\.shp$", full.names = TRUE)
shapefiles<-shapefiles[1:length(shapefiles)-1]

# Read each shapefile, transform it, and combine them into a single object
Hokken <- lapply(shapefiles, function(file) {
  # Read the shapefile
  sf_obj <- read_sf(file)%>%
    # Transform it
    st_transform("EPSG:31370")
}) 

colnames(Hokken[[1]])[4]<-"Nummering"
colnames(Hokken[[2]])[4]<-"Nummering"

# merge them
Hokken_merged<-do.call(rbind, Hokken)

plot(Hokken_merged)

#read province data

Provinces<-read_sf("C:/Users/michiel_lathouwers/Downloads/Provincies_shapefile/BELGIUM_-_Provinces.shp")
Provinces<-st_transform(Provinces,"EPSG:31370")

#Get province name to Hokken_merged

Hokken_centroid<-st_centroid(Hokken_merged)
Hokken_centroid_provinces<-st_join(Hokken_centroid,Provinces)
Hokken_merged$Province<-Hokken_centroid_provinces$NE_Name

#remove hokken without numbering

Hokken_merged_subset<-Hokken_merged[which(is.na(Hokken_merged$Nummering)==FALSE),]

#organisatie aanpassen
Hokken_merged_subset$Organisati[which(is.na(Hokken_merged_subset$Organisati)==TRUE)]<-"Ander"
#tabel
table(Hokken_merged_subset$Province,Hokken_merged_subset$Organisati)