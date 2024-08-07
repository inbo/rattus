
library(ggplot2)
library(sf)
library(tidyverse)

# overzicht staalname resistentie

shapefiles <- list.files(path = "data/kaarten VMM shapefiles",
                         pattern = "\\.shp$", full.names = TRUE)

# Read each shapefile, transform it, and combine them into a single object
Hokken <- lapply(shapefiles, function(file) {
  # Read the shapefile
  sf_obj <- read_sf(file)%>%
    # Transform it
    st_transform("EPSG:31370")
}) 
#Fix capital in nummering

colnames(Hokken[[1]])[4]<-"Nummering"
colnames(Hokken[[2]])[4]<-"Nummering"

#Add two letter Bekken code
Bekkens<-c("BE","BO","BP","DE","DD","DL","GK","IZ","LE","MA","ML","NE")

for (i in 1:length(Hokken)) {
  Hokken[[i]]$Bekken<-rep(Bekkens[i],length.out=nrow(Hokken[[i]]))
  
}

# merge them
Hokken_merged<-do.call(rbind, Hokken)

plot(Hokken_merged)

#read province data

Provinces<-read_sf("data/Provincies_shapefile/BELGIUM_-_Provinces.shp")
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

# merge bekken en nummering for Hok (with leading zero)
Hokken_merged_subset$Nummering<-as.character(Hokken_merged_subset$Nummering)
for (i in 1:nrow(Hokken_merged_subset)) {
  if (nchar(Hokken_merged_subset$Nummering[i]) == 1) {
  # Add a leading zero
    Hokken_merged_subset$Nummering[i] <- paste0("0", Hokken_merged_subset$Nummering[i])
}
}

Hokken_merged_subset$Hok<-paste0(Hokken_merged_subset$Bekken,Hokken_merged_subset$Nummering)


### Staalname Vlaams Brabant

# Load the googlesheets4 package
library(googlesheets4)

# Authenticate with Google (this will open a browser window for you to log in)
gs4_auth()

# Specify the URL or the sheet ID of the Google Spreadsheet
url <- "https://docs.google.com/spreadsheets/d/1jVfoSk7o894IAEuVZP1WSLU52GyvzbvhH6xjAHwPSBU/edit?gid=0#gid=0"

# Read the data from the Google Spreadsheet into R
data_staalname <- read_sheet(url)

# Subset hokken Vlaams Brabant
Hokken_merged_VlBr<-Hokken_merged_subset[which(Hokken_merged_subset$Province=="Vlaams Brabant"),]

# Check for presence of Hok in data_staalname and add a new column
Hokken_merged_VlBr <- Hokken_merged_VlBr %>%
  mutate(Hok_present = Hok %in% data_staalname$Hok)

table(Hokken_merged_VlBr$Hok_present)
120/165

# Plot the map using ggplot2
ggplot() +
  # Plot Province
  geom_sf(data = Provinces[which(Provinces$NAME_2=="Vlaams Brabant"),], fill = NA, color = "black", size = 0.5) +
  theme_minimal() +
  # Plot Hokken_merged_VlBr
  geom_sf(data = Hokken_merged_VlBr, aes(fill = Hok_present), alpha=0.4) +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "green"), 
                    name = "Verzameld?") +
  theme(legend.position = "bottom")
