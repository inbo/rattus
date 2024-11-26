
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

# Read bekkens

Bekkens<-read_sf("data/Bekkens_shapefile/Wsbekken.shp")
Bekkens<-st_transform(Bekkens,"EPSG:31370")
plot(Bekkens)

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
  #geom_sf(data = Provinces[which(Provinces$FIRST_NAME=="Vlaanderen"|Provinces$FIRST_NAME=="Bruxelles" ),], fill = NA, color = "black", size = 0.5) +
  # Plot bekkens
  geom_sf(data=Bekkens, fill=NA, color="darkslategrey",size=1)+
  theme_void() +
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


# Perform the left join with only the ID column
Brussel_subset <- left_join(Brussel_subset, data_staalname_subset, by = "Hok")

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
#View(data_staalname_updated)
#copy paste to google spreadsheet, and do brussels manually

#write.csv(data_staalname_updated$Lat_Centr.y, "Lat_Centr.csv", row.names = FALSE)
#write.csv(data_staalname_updated$Lon_Centr.y, "Lon_Centr.csv", row.names = FALSE)

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

#Add genotype data to hokken
# Select only the Genotype and Hok columns from Genotype data
Data_Genotyping$ID
Data_Genotyping_subset <- Data_Genotyping %>% select(ID, Call_final_Kristof)

# Perform the left join with only the genotype column
Hokken_Genotype<- left_join(Hokken_merged_subset, Data_Genotyping_subset, by = "ID")

# Perform the left join with only the genotype column BRUSSELS
Hokken_Genotype_Brussels<- left_join(Brussel_subset, Data_Genotyping_subset, by = "ID")

#Clean up
Hokken_Genotype$Call_final_Kristof[which(Hokken_Genotype$Call_final_Kristof=="M1M1?")]<-"M1M1"

Hokken_Genotype$Call_final_Kristof[which(Hokken_Genotype$Call_final_Kristof=="MissingData")]<-NA
Hokken_Genotype$Call_final_Kristof[which(Hokken_Genotype$Call_final_Kristof=="NoData")]<-NA

# Remove duplicate rows based on the Hok column, keeping the first occurrence
Hokken_Genotype <- Hokken_Genotype %>%
  distinct(Hok, .keep_all = TRUE)

# Custom colors for the Call_final factor levels
custom_colors <- c("NA" = "grey", 
                   "M1W" = "coral",     # light red-pink-ish
                   "M1M1" = "red", 
                   "WW" = "darkslategrey", 
                   "M2W" = "cornflowerblue", 
                   "SPECIAAL" = "orange", 
                   "M3W" = "yellow", 
                   "M2M2" = "navy")

# Plot with custom colors
ggplot() +
  # Plot Province
  geom_sf(data = Provinces[which(Provinces$FIRST_NAME == "Vlaanderen" | Provinces$FIRST_NAME == "Bruxelles"),], 
          fill = NA, color = "black", size = 0.5) +
  # Plot Hokken_merged_subset
  geom_sf(data = Hokken_Genotype, aes(fill = Call_final_Kristof), alpha = 0.6) +
  geom_sf(data = Hokken_Genotype_Brussels,aes(fill = Call_final_Kristof), alpha = 0.6)+
  # Manually set colors for Call_final factor levels
  scale_fill_manual(values = custom_colors, na.value = "lightgrey") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),      # Remove grid
    axis.text = element_blank(),       # Remove axis tick labels
    axis.ticks = element_blank()       # Remove axis ticks
  )

#Check
subset(Hokken_Genotype, Hokken_Genotype$ID=="RAT-0517")
table(Hokken_Genotype$Bekken)

#Plot the samples which need to be retested
Data_Genotyping_subset <- Data_Genotyping %>% select(ID, Call_final_Kristof)

# Perform the left join with only the genotype column
Hokken_Genotype<- left_join(Hokken_merged_subset, Data_Genotyping_subset, by = "ID")

Hokken_Genotype_Missing<-Hokken_Genotype %>%
  filter(Call_final_Kristof == "MissingData" | Call_final_Kristof == "NoData")

#Plot
ggplot() +
  # Plot Province
  geom_sf(data = Provinces[which(Provinces$FIRST_NAME == "Vlaanderen" | Provinces$FIRST_NAME == "Bruxelles"),], 
          fill = NA, color = "black", size = 0.5) +
  # Plot Hokken_merged_subset
  geom_sf(data = Hokken_Genotype_Missing, aes(fill = Call_final_Kristof), alpha = 0.6) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),      # Remove grid
    axis.text = element_blank(),       # Remove axis tick labels
    axis.ticks = element_blank()       # Remove axis ticks
  )

nrow(Hokken_Genotype_Missing)

#Add genotype data to google spreadsheet staalname 2024
data_staalname
Data_Genotyping

Data_Genotyping_subset <- Data_Genotyping %>% select(ID, Call_final_Kristof)
Data_Staalname_Genotype<- left_join(data_staalname
                                    , Data_Genotyping_subset, by = "ID")

Data_Staalname_Genotype <- Data_Staalname_Genotype %>%
  distinct(ID, .keep_all = TRUE)

#write.csv(cbind.data.frame(Data_Staalname_Genotype$ID,
 #                          Data_Staalname_Genotype$Hok,
 #                          Data_Staalname_Genotype$Call_final_Kristof), "Staalname_Genotype.csv", row.names = FALSE)

#Plot resistence with points instead of 
data_staalname_df<-as.data.frame(data_staalname)
data_staalname_df$Lat<- as.numeric(sapply(data_staalname_df$Lat, function(x) if (is.null(x)) NA else x))
data_staalname_df$Lon<- as.numeric(sapply(data_staalname_df$Lon, function(x) if (is.null(x)) NA else x))
data_staalname_df$Lat_Centr<- as.numeric(sapply(data_staalname_df$Lat_Centr, function(x) if (is.null(x)) NA else x))
data_staalname_df$Lon_Centr<- as.numeric(sapply(data_staalname_df$Lon_Centr, function(x) if (is.null(x)) NA else x))

# Loop for putting centroid coordinates when real are missing
for (i in 1:nrow(data_staalname_df)) {
  # Check if Lat is NA, and if so, copy Lat_Centr to Lat
  if (is.na(data_staalname_df$Lat[i])) {
    data_staalname_df$Lat[i] <- data_staalname_df$Lat_Centr[i]
  }
  
  # Check if Lon is NA, and if so, copy Lon_Centr to Lon
  if (is.na(data_staalname_df$Lon[i])) {
    data_staalname_df$Lon[i] <- data_staalname_df$Lon_Centr[i]
  }
}

#Clean up
data_staalname_df$Genotype[which(data_staalname_df$Genotype=="M1M1?")]<-"M1M1"

data_staalname_df$Genotype[which(data_staalname_df$Genotype=="MissingData")]<-NA
data_staalname_df$Genotype[which(data_staalname_df$Genotype=="NoData")]<-NA


# Custom colors for the Call_final factor levels
custom_colors <- c(#"NA" = "grey", 
                   "M1W" = "coral",     # light red-pink-ish
                   "M1M1" = "red", 
                   "WW" = "darkslategrey", 
                   "M2W" = "cornflowerblue", 
                   "M3W" = "yellow", 
                   "M2M2" = "navy",
                   "M1M2" = "darkviolet",
                   "M1M3" = "orange")

# Filter out rows with NA in Lat or Lon
data_staalname_df_no_na <- data_staalname_df %>%
  filter(!is.na(Lat) & !is.na(Lon)& !is.na(Genotype))

#View(data_staalname_df)

# Convert the filtered dataframe to an sf object
data_staalname_sf <- st_as_sf(data_staalname_df_no_na, coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)

# Plot the background map and points
ggplot() +
  # Plot Provinces
  geom_sf(data = Provinces[Provinces$FIRST_NAME %in% c("Vlaanderen", "Bruxelles"),], 
          fill = NA, color = "black", size = 0.5) +
  # Plot points with colors based on Genotype
  geom_sf(data = data_staalname_sf, aes(color = Genotype), size = 2) +
  # Customize color scale with custom colors
  scale_color_manual(values = custom_colors) +
  labs(color = "Genotype") +
  theme_minimal()

# Plot the background map and points, now with bekken
ggplot() +
  geom_sf(data = Bekkens, fill = NA, color = "black", size = 1.5) +
  geom_sf(data = data_staalname_sf, aes(color = Genotype), size = 2) +
  scale_color_manual(values = custom_colors) +
  labs(color = "Genotype") +
  theme_void() +
  theme(legend.position = "bottom")

table(data_staalname_sf$Genotype)

#Plot hokken with new genotypes
# Loop through each row of Hokken_Genotype
Hokken_Genotype$Genotype<-rep(NA,length.out=nrow(Hokken_Genotype))
for (i in 1:nrow(Hokken_Genotype)) {
  # Find the index in data_staalname_df where IDs match
  match_index <- which(data_staalname_df$ID == Hokken_Genotype$ID[i])
  
  # If a match is found, update Genotype, otherwise leave as is
  if (length(match_index) > 0) {
    Hokken_Genotype$Genotype[i] <- data_staalname_df$Genotype[match_index]
  }
}

#Same for Brussels
Hokken_Genotype_Brussels$Genotype<-rep(NA,length.out=nrow(Hokken_Genotype_Brussels))
for (i in 1:nrow(Hokken_Genotype_Brussels)) {
  # Find the index in data_staalname_df where IDs match
  match_index <- which(data_staalname_df$ID == Hokken_Genotype_Brussels$ID[i])
  
  # If a match is found, update Genotype, otherwise leave as is
  if (length(match_index) > 0) {
    Hokken_Genotype_Brussels$Genotype[i] <- data_staalname_df$Genotype[match_index]
  }
}

#Clean up NA's
Hokken_Genotype$Genotype[which(is.na(Hokken_Genotype$Genotype)==TRUE)]<-""
View(Hokken_Genotype)

# #Update plot 

Hokken_Genotype$Genotype <- as.character(Hokken_Genotype$Genotype)  # Convert to character
Hokken_Genotype$Genotype[is.na(Hokken_Genotype$Genotype)] <- "Geen gegevens"  # Replace NA with label
Hokken_Genotype$Genotype[which(Hokken_Genotype$Genotype=="NA")] <-"Geen gegevens"
Hokken_Genotype$Genotype[which(Hokken_Genotype$Genotype=="")]<-"Geen gegevens"
Hokken_Genotype$Genotype <- factor(Hokken_Genotype$Genotype)  # Convert back to factor

unique(Hokken_Genotype$Genotype)

Hokken_Genotype_Brussels$Genotype <- as.character(Hokken_Genotype_Brussels$Genotype)  # Repeat for Brussels data
Hokken_Genotype_Brussels$Genotype[is.na(Hokken_Genotype_Brussels$Genotype)] <- "Geen gegevens"
Hokken_Genotype_Brussels$Genotype <- factor(Hokken_Genotype_Brussels$Genotype)


# Custom colors for the Call_final factor levels
custom_colors <- c(
  "M1M1" = "#ff0000",     # Adjusted from #f87e7e
  "M1W" = "#ff44cc",    # Adjusted from #f888c8
  "WW" = "#cccccc",      # Adjusted from #dcdddc
  "M2W" = "#66ffff",     # Adjusted from #a9f9fd
  "M3W" = "#ffff66",     # Adjusted from #fdf9c4
  "M2M2" = "#6666ff",    # Adjusted from #828aff
  "M1M2" = "#9966ff",    # Adjusted from #cb93f7
  "M1M3" = "#ffcc66",    # Adjusted from #fad07e
  "Geen gegevens" = "#2f4f4f" # Retained darkslategrey
)

# Load forcats package for reordering factors
library(forcats)

# Reorder levels of Genotype in both datasets
Hokken_Genotype$Genotype <- fct_relevel(Hokken_Genotype$Genotype, "Geen gegevens", after = Inf)
Hokken_Genotype_Brussels$Genotype <- fct_relevel(Hokken_Genotype_Brussels$Genotype, "Geen gegevens", after = Inf)


ggplot() +
  # Plot Hokken_merged_subset
  geom_sf(data = Hokken_Genotype, aes(fill = Genotype), alpha = 0.6) +
  geom_sf(data = Hokken_Genotype_Brussels, aes(fill = Genotype), alpha = 0.6) +
  # Plot bekken outlines
  geom_sf(data = Bekkens, fill = NA, color = "black", size = 1.5) +
  # Manually set colors for Genotype levels
  scale_fill_manual(
    values = custom_colors,  # Updated custom colors
    na.translate = FALSE      # Ensure NA is not included separately
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),      # Remove grid
    axis.text = element_blank(),       # Remove axis tick labels
    axis.ticks = element_blank()       # Remove axis ticks
  )

# Descriptive statistics staalname resistentie

# Specify the URL or the sheet ID of the Google Spreadsheet
url <- "https://docs.google.com/spreadsheets/d/1jVfoSk7o894IAEuVZP1WSLU52GyvzbvhH6xjAHwPSBU/edit?gid=0#gid=0"

# Read the data from the Google Spreadsheet into R
data_staalname <- read_sheet(url)

# Filter data
data_staalname<-data_staalname[which(is.na(data_staalname$Hok)==F),]
data_staalname<-data_staalname[which(is.na(data_staalname$Genotype)==F),]
data_staalname<-data_staalname[which(data_staalname$Genotype!="NA"),]

# Summarize genotype
table(data_staalname$Genotype)
nrow(data_staalname)






