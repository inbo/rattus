
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

# Do the same for entire sampling campaign

plot(Hokken_merged_subset)
data_staalname$Hok

Hokken_merged_subset <- Hokken_merged_subset %>%
  mutate(Hok_present = Hok %in% data_staalname$Hok)

# Select only the ID and Hok columns from data_staalname
data_staalname_subset <- data_staalname %>% select(Hok, ID)

# Perform the left join with only the ID column
Hokken_merged_subset <- left_join(Hokken_merged_subset, data_staalname_subset, by = "Hok")

ggplot() +
  # Plot Province
  geom_sf(data = Provinces[which(Provinces$FIRST_NAME=="Vlaanderen"|Provinces$FIRST_NAME=="Bruxelles" ),], fill = NA, color = "black", size = 0.5) +
  theme_minimal() +
  # Plot Hokken_merged_subset
  geom_sf(data = Hokken_merged_subset, aes(fill = Hok_present), alpha=0.6) +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "green"), 
                    name = "Verzameld?") +
  theme(legend.position = "bottom")

#Add brussels
# overzicht staalname resistentie

shapefiles <- list.files(path = "data/kaart Brussel",
                         pattern = "\\.shp$", full.names = TRUE)

# Read  shapefile
Brussel <- lapply(shapefiles, function(file) {
  # Read the shapefile
  sf_obj <- read_sf(file)
})

multipolygons<-Brussel[[1]]
points<-Brussel[[2]]

joined_data <- st_join(multipolygons, points, join = st_contains)

# Add Hok column
multipolygons_with_nummering <- joined_data %>%
  mutate(Nummering = fid) %>%
  select(-fid)

Brussel_subset<-multipolygons_with_nummering[which(is.na(multipolygons_with_nummering$Nummering)==FALSE),]
Brussel_subset$Hok<-paste0("BR",Brussel_subset$Nummering)

#Presence

Brussel_subset <- Brussel_subset %>%
  mutate(Hok_present = Hok %in% data_staalname$Hok)

#plot
ggplot() +
  # Plot Province
  geom_sf(data = Provinces[which(Provinces$FIRST_NAME == "Vlaanderen" | Provinces$FIRST_NAME == "Bruxelles"),], fill = NA, color = "black", size = 0.5) +
  # Plot Hokken_merged_subset
  geom_sf(data = Hokken_merged_subset, aes(fill = Hok_present), alpha = 0.6) +
  geom_sf(data = Brussel_subset, aes(fill = Hok_present), alpha = 0.6) +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "green"), 
                    name = "Staartpunt verzameld",
                    labels = c("FALSE" = "Nee", "TRUE" = "Ja")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),      # Remove grid
    axis.text = element_blank(),       # Remove axis tick labels
    axis.ticks = element_blank()       # Remove axis ticks
  )

## Extract centroid coordinates
Hokken_centroid

# merge bekken en nummering for Hok (with leading zero)
Hokken_centroid$Nummering <- as.character(Hokken_centroid$Nummering)
Hokken_centroid$Nummering <- ifelse(nchar(Hokken_centroid$Nummering) == 1, 
                                    paste0("0", Hokken_centroid$Nummering), 
                                    Hokken_centroid$Nummering)

Hokken_centroid$Hok<-paste0(Hokken_centroid$Bekken,Hokken_centroid$Nummering)

#transform the CRS to WGS84
Hokken_centroid<- st_transform(Hokken_centroid, crs = 4326)

# Extract the longitude and latitude from the geometry column and add them as new columns
Hokken_centroid$Lon_Centr <- st_coordinates(Hokken_centroid)[, 1]
Hokken_centroid$Lat_Centr <- st_coordinates(Hokken_centroid)[, 2]

# Perform  left join to add Lat_Centr and Lon_Centr to data_staalname
data_staalname_updated <- data_staalname %>%
  left_join(select(Hokken_centroid, Hok, Lat_Centr, Lon_Centr), by = "Hok")

#copy paste to google spreadsheet, and do brussels manually

#write.csv(data_staalname_updated$Lat_Centr, "Lat_Centr.csv", row.names = FALSE)
#write.csv(data_staalname_updated$Lon_Centr, "Lon_Centr.csv", row.names = FALSE)

# Transform the CRS to WGS84
#points_wgs84 <- st_transform(points, crs = 4326)
#View(points_wgs84)

#Updated overview of sampling effort based on google sheet 

# Get unique values in "Hok"
unique_hok_values <- unique(na.omit(data_staalname_updated$Hok))

# Derive "Bekken" by taking the first two letters of "Hok"
sampling_bekken <- substr(unique_hok_values, 1, 2)

# Create a table with the counts of each unique "Bekken"
sampling_bekken_counts <- table(na.omit(sampling_bekken))

#######################################################

# Read genotyping data
Data_Genotyping<-read.csv("data/Resistentie_Genotyping.csv",sep=";",header=T)

#Read LIMS data
Data_LIMS<-read.csv("data/Resistentie_LIMS.csv",sep=";",header=T)

#Fix format
Data_LIMS$ID_LIMS<- gsub("-","__",Data_LIMS$ID_LIMS)
Data_LIMS$ID_LIMS<- gsub("_1","_01",Data_LIMS$ID_LIMS)

#Add ID to genotyping data ## IT'S ALREADY IN THERE
Data_Genotyping

#Add genotype data to hokken
Hokken_merged_subset

# Select only the Genotype and Hok columns from Genotype data
Data_Genotyping$ID<-Data_Genotyping$ExternSampleID
Data_Genotyping_subset <- Data_Genotyping %>% select(ExternSampleID, Call_final)

# Perform the left join with only the genotype column
test <- left_join(Hokken_merged_subset, data_staalname_subset, by = "Hok")

#plot
ggplot() +
  # Plot Province
  geom_sf(data = Provinces[which(Provinces$FIRST_NAME == "Vlaanderen" | Provinces$FIRST_NAME == "Bruxelles"),], fill = NA, color = "black", size = 0.5) +
  # Plot Hokken_merged_subset
  geom_sf(data = Hokken_merged_subset, aes(fill = Hok_present), alpha = 0.6) +
  geom_sf(data = Brussel_subset, aes(fill = Hok_present), alpha = 0.6) +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "green"), 
                    name = "Staartpunt verzameld",
                    labels = c("FALSE" = "Nee", "TRUE" = "Ja")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),      # Remove grid
    axis.text = element_blank(),       # Remove axis tick labels
    axis.ticks = element_blank()       # Remove axis ticks
  )





