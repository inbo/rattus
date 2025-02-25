
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
                   "M1W" = "deeppink",     # light red-pink-ish
                   "M1M1" = "red", 
                   "WW" = "grey", 
                   "M2W" = "turquoise1", 
                   "M3W" = "khaki1", 
                   "M2M2" = "blue",
                   "M1M2" = "purple",
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

# pie chart

# Count occurrences of each Genotype
genotype_counts <- data_staalname_sf %>%
  count(Genotype)


# Create the pie chart
ggplot(genotype_counts, aes(x = "", y = n, fill = Genotype)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +  # Convert to a pie chart
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme_void()   # Remove background elements) 

# bar chart

Delen


Je zei:
  in dit document staat een paragraaf rond densiteit afhankelijke effecten. vat samen en schrijf het uit voor mijn rapport, geen bulletpoints en subtitels gewoon een vlotte wetenschappelijke beschrijving van het concept van densiteitsafhankelijke effecten.
ChatGPT zei:
  Densiteitsafhankelijke effecten verwijzen naar de processen binnen een populatie waarvan de intensiteit afhangt van de dichtheid van individuen. Naarmate de populatiedichtheid toeneemt, kunnen verschillende biotische factoren zoals competitie, predatie, parasitisme en ziekten een sterkere invloed uitoefenen op de groei en overleving van individuen. Dit mechanisme speelt een cruciale rol in de regulatie van populaties en helpt extreme fluctuaties te voorkomen door negatieve terugkoppeling.

Bij hoge dichtheden neemt de competitie om schaarse hulpbronnen, zoals voedsel, water en ruimte, toe, wat leidt tot een afname van de individuele fitness en een verhoogde ster







Je zei:
  Newton.Populationlimitationinbirds.2007.BB.pdf
PDF
hier is het document
Densiteitsafhankelijke effecten spelen een cruciale rol in de regulatie van populaties en zorgen ervoor dat populatieniveaus binnen bepaalde grenzen blijven fluctueren. Deze effecten treden op wanneer de intensiteit van ecologische processen, zoals competitie, predatie en ziekteverspreiding, afhankelijk is van de populatiedichtheid. Wanneer een populatie groeit, nemen de interacties tussen individuen toe, wat kan leiden tot een verhoogde sterfte en een verminderde reproductie, waardoor de groei van de populatie wordt afgeremd.

Competitie om hulpbronnen, zoals voedsel en nestplaatsen, is een belangrijk mechanisme van densiteitsafhankelijke regulatie. Naarmate de dichtheid stijgt, wordt de toegang tot voedsel beperkter, waardoor individuen minder energie beschikbaar hebben voor voortplanting en overleving. Daarnaast kan verhoogde populatiedichtheid leiden tot een grotere blootstelling aan predatoren, omdat prooidieren zich in grotere aantallen op eenzelfde locatie bevinden, wat predatoren efficiënter maakt in hun jacht. Ook ziektes en parasieten kunnen zich sneller verspreiden in dichtbevolkte populaties, wat de sterftecijfers verder verhoogt.

In vogelpopulaties zijn verschillende voorbeelden van densiteitsafhankelijke regulatie gedocumenteerd. Zo blijft de broeddichtheid van sommige soorten, zoals de Sperwer (Accipiter nisus), relatief stabiel over de jaren doordat de rekrutering van nieuwe broedvogels in evenwicht wordt gehouden met het verlies van gevestigde broedparen. Bij andere soorten, zoals het Korhoen (Lagopus lagopus), blijken sterftecijfers in de winter hoger te zijn op locaties met een hogere dichtheid, voornamelijk door verhoogde predatiedruk. Daarnaast kan competitie voor nestplaatsen, zoals bij de Amerikaanse Hout-eend (Aix sponsa), resulteren in een verminderde voortplantingssucces bij hoge dichtheden​
.

Ondanks deze regulerende mechanismen zijn er situaties waarin populaties niet stabiliseren maar sterke fluctuaties vertonen. Dit kan gebeuren wanneer er sprake is van omgekeerde of vertraagde densiteitsafhankelijke effecten, waarbij de negatieve gevolgen pas later optreden. Zo kan een populatie aanvankelijk groeien zonder merkbare negatieve effecten, maar wanneer de dichtheid een kritische drempel overschrijdt, kunnen sterftecijfers plotseling stijgen of reproductiesuccessen dalen, wat resulteert in een scherpe afname van de populatiegrootte​
.

Het bestuderen van densiteitsafhankelijke effecten vereist lange termijn datasets, omdat de invloed van dichtheid op populatiedynamiek vaak moeilijk te onderscheiden is van densiteitsonafhankelijke factoren zoals klimaatschommelingen. Desondanks heeft uitgebreid onderzoek aangetoond dat densiteitsafhankelijke processen een fundamentele rol spelen in de populatieregulatie van vogels en vele andere diersoorten, en dat deze mechanismen cruciaal zijn voor het begrijpen van ecologische stabiliteit en soortendynamiek​
.







Je zei:
  heb je een ander woord voor hulpbronnen?
  ChatGPT zei:
  Ja, afhankelijk van de context kun je de volgende synoniemen gebruiken voor "hulpbronnen":
  
  Middelen (bijv. voedselmiddelen, leefmiddelen)
Voorraden (bijv. voedselvoorraden)
Benodigdheden (meer algemeen, afhankelijk van de context)
Beschikbare rijkdommen (in bredere ecologische zin)
Grondstoffen (voor een meer fysiek of economisch perspectief)
Levensbronnen (meer biologisch georiënteerd)
Voor jouw tekst zou middelen of voorraden goed passen, afhankelijk van hoe je het wilt formuleren. Bijvoorbeeld: "Competitie om middelen zoals voedsel en nestplaatsen speelt een belangrijke rol in populatieregulatie."







Je zei:
  frauendorf_love_2022.pdf
PDF
kan je van de studie in bijlage in een paar zinnen samenvatten wel effect de densiteit van de broedpopulatie van oystercatchers heeft op predatiedruk? 
  De studie van Frauendorf et al. (2022) onderzoekt hoe de dichtheid van broedpopulaties van scholeksters (Haematopus ostralegus) de predatiedruk beïnvloedt en toont aan dat deze relatie sterk afhankelijk is van de samenstelling van de predatorgemeenschap. In gebieden waar aviaire predatoren (zoals meeuwen en kiekendieven) dominant zijn, neemt de nestoverleving toe bij een hogere dichtheid van broedende scholeksters. Dit wordt verklaard door het gezamenlijke ‘mobbing’-gedrag van de vogels, waarbij ze roofvogels effectief kunnen verjagen.

Daarentegen leidt een hogere nestdichtheid in gebieden met veel zoogdierpredatoren (zoals vossen en marters) tot een lagere nestoverleving. Zoogdieren maken namelijk gebruik van gebiedsgebonden zoekstrategieën, waardoor ze clusters van nesten efficiënter kunnen lokaliseren en exploiteren. Dit resulteert in een negatieve densiteitsafhankelijke overleving, waarbij een grotere concentratie nesten juist meer predatie aantrekt.

Opvallend is dat deze negatieve densiteitsafhankelijke effecten niet worden waargenomen op roofdier-vrije eilanden, waar scholeksters profiteren van het ontbreken van zoogdierpredatie. De studie onderstreept daarmee dat het effect van nestdichtheid op predatie afhangt van de lokale predatoren en suggereert dat beheersmaatregelen voor broedvogels effectiever zijn in gebieden met een lagere zoogdierpredatie​
.







Je zei:
  verbeter, zonder de verwoording aan te passen "Densiteitsafhankelijke effecten spelen een cruciale rol in de regulatie van populaties door het imapcteren van populaiteparameters zoals overleving, voortplantingssucces en emmigratie. Deze effecten spelen bij zowel de potentiële prooisoorten als de predatorsoorten die in dit rapport besproken worden. Deze effecten treden op wanneer de intensiteit van ecologische processen afhankelijk is van de populatiedichtheid (Newton, 1998). Competitie, voor bijvoorbeeld voedsel en nestgelegenheid, is een belangrijk mechanisme van densiteitsafhankelijke regulatie. Naarmate de dichtheid van een populatie stijgt, wordt de toegang tot gelimiteerde voedselbronnen of broedplaatsen beperkt, hetgeen negatieve effecten kan hebben op voortplanting en overleving. Daarnaast kan verhoogde populatiedichtheid leiden tot een verhoogde predatiedruk. Bij hoge aantallen van een bepaalde prooisoort zullen individuen van anders generalistische predatoren zich specialiseren in die talrijke soort. Ook ziektes en parasieten kunnen zich sneller verspreiden in dichtbevolkte populaties, wat de sterftecijfers verder kan verhogen. Negatieve densiteitsafhankelijke effecten werden bijvoorbeeld aangetoond voor broeddpopulaties van patrijs in Frankrijk (Bro et al., ) en Polen (Panek et al., 1997), waar vooral de toename in competitie voor voedsel en geschikte broedterritoria een negatief effect blijken te hebben op broedsucces
Ondanks deze regulerende mechanismen zijn er situaties waarin populaties niet stabiliseren maar sterke fluctuaties vertonen. Dit kan gebeuren wanneer er sprake is van vertraagde densiteitsafhankelijke effecten, waarbij de negatieve gevolgen pas later optreden. Zo kan een populatie aanvankelijk groeien zonder merkbare negatieve effecten, maar wanneer de dichtheid een kritische drempel overschrijdt, kunnen sterftecijfers plotseling stijgen of reproductiesuccessen dalen, wat resulteert in een scherpe afname van de populatiegrootte (Newton 1998)​. Ook kunnen er omgekeerde densiteitsafhankelijke effecten optreden waarbij een populatie onder een bepaalde densiteit negatieve gevolgen ondervind. Een studie van Frauendorf et al. (2022) onderzoekt hoe de dichtheid van broedpopulaties van scholeksters de predatiedruk beïnvloedt en toont aan dat deze relatie sterk afhankelijk is van de samenstelling van de predatorgemeenschap. In gebieden waar vogel predatoren (zoals meeuwen en kiekendieven) dominant zijn, neemt de nestoverleving toe bij een hogere dichtheid van broedende scholeksters. Dit wordt verklaard door het gezamenlijke ‘mobbing’-gedrag van de vogels, waarbij ze roofvogels effectief kunnen verjagen. Daarentegen leidt een hogere nestdichtheid in gebieden met veel zoogdierpredatoren (zoals vossen en marters) tot een lagere nestoverleving. Zoogdieren maken namelijk gebruik van gebiedsgebonden zoekstrategieën, waardoor ze clusters van nesten efficiënter kunnen lokaliseren en exploiteren. Dit resulteert in een negatieve densiteitsafhankelijke overleving, waarbij een grotere concentratie nesten juist meer predatie aantrekt.
"
ChatGPT zei:
  Densiteitsafhankelijke effecten spelen een cruciale rol in de regulatie van populaties door het beïnvloeden van populatieparameters zoals overleving, voortplantingssucces en emigratie. Deze effecten zijn relevant voor zowel de potentiële prooisoorten als de predatorsoorten die in dit rapport besproken worden. Ze treden op wanneer de intensiteit van ecologische processen afhankelijk is van de populatiedichtheid (Newton, 1998). Competitie, bijvoorbeeld voor voedsel en nestgelegenheid, is een belangrijk mechanisme van densiteitsafhankelijke regulatie. Naarmate de populatiedichtheid stijgt, wordt de toegang tot schaarse voedselbronnen of geschikte broedplaatsen beperkt, wat negatieve effecten kan hebben op voortplanting en overleving. Daarnaast kan een hogere populatiedichtheid leiden tot verhoogde predatiedruk. Wanneer een bepaalde prooisoort in grote aantallen voorkomt, kunnen anders generalistische predatoren zich specialiseren in die soort. Ook ziektes en parasieten verspreiden zich sneller in dichtbevolkte populaties, wat de sterftecijfers verder kan verhogen. Negatieve densiteitsafhankelijke effecten werden bijvoorbeeld aangetoond bij broedpopulaties van patrijzen in Frankrijk (Bro et al.) en Polen (Panek et al., 1997), waar vooral de toename in competitie voor voedsel en geschikte broedterritoria een negatief effect blijkt te hebben op het broedsucces.

Ondanks deze regulerende mechanismen zijn er situaties waarin populaties niet stabiliseren maar sterke fluctuaties vertonen. Dit kan gebeuren wanneer er sprake is van vertraagde densiteitsafhankelijke effecten, waarbij de negatieve gevolgen pas later optreden. Zo kan een populatie aanvankelijk groeien zonder merkbare negatieve effecten, maar wanneer de dichtheid een kritische drempel overschrijdt, kunnen sterftecijfers plotseling stijgen of het voortplantingssucces dalen, wat resulteert in een scherpe afname van de populatiegrootte (Newton, 1998). Daarnaast kunnen omgekeerde densiteitsafhankelijke effecten optreden, waarbij een populatie onder een bepaalde dichtheidsdrempel negatieve gevolgen ondervindt.

Een studie van Frauendorf et al. (2022) onderzoekt hoe de dichtheid van broedpopulaties van scholeksters de predatiedruk beïnvloedt en toont aan dat deze relatie sterk afhankelijk is van de samenstelling van de predatorgemeenschap. In gebieden waar aviaire predatoren (zoals meeuwen en kiekendieven) dominant zijn, neemt de nestoverleving toe bij een hogere dichtheid van broedende scholeksters. Dit wordt verklaard door het gezamenlijke ‘mobbing’-gedrag van de vogels, waarbij ze roofvogels effectief kunnen verjagen. Daarentegen leidt een hogere nestdichtheid in gebieden met veel zoogdierpredatoren (zoals vossen en marters) tot een lagere nestoverleving. Zoogdieren maken namelijk gebruik van gebiedsgebonden zoekstrategieën, waardoor ze clusters van nesten efficiënter kunnen lokaliseren en exploiteren. Dit resulteert in een negatieve densiteitsafhankelijke overleving, waarbij een grotere concentratie nesten juist meer predatie aantrekt.







Je zei:
  I have these data in R "table(data_staalname_sf$Genotype) - M1M1 M1M2 M1M3  M1W M2M2  M2W  M3W   WW 
 169    5    1  250   29   45    5  486 " can you make me a pie chart of this using these colours? "# Custom colors for the Call_final factor levels
custom_colors <- c(#"NA" = "grey", 
                   "M1W" = "deeppink",     # light red-pink-ish
                   "M1M1" = "red", 
                   "WW" = "grey", 
                   "M2W" = "turquoise1", 
                   "M3W" = "khaki1", 
                   "M2M2" = "blue",
                   "M1M2" = "purple",
                   "M1M3" = "orange")"
ChatGPT zei:
  It looks like there was an issue displaying the chart. Let me try again. ​​

It seems there is a technical issue preventing the display of the chart. Let me save the chart as an image and provide you with a download link. ​​

There seems to be a persistent issue preventing me from displaying or saving the pie chart. I recommend you try running the following R code in your local environment to generate the pie chart:
  
  r
Kopiëren
Bewerken
# Define the data
genotype_counts <- c("M1M1" = 169, "M1M2" = 5, "M1M3" = 1, "M1W" = 250,
                     "M2M2" = 29, "M2W" = 45, "M3W" = 5, "WW" = 486)

# Define the custom colors
custom_colors <- c("M1W" = "deeppink", "M1M1" = "red", "WW" = "grey",
                   "M2W" = "turquoise1", "M3W" = "khaki1", "M2M2" = "blue",
                   "M1M2" = "purple", "M1M3" = "orange")

# Generate the pie chart
pie(genotype_counts, col = custom_colors[names(genotype_counts)],
    main = "Genotype Distribution")
This should correctly create the pie chart with your desired colors. Let me know if you need further adjustments! ​​







Je zei:
  give script for ggplot
ChatGPT zei:
  Here is an R script using ggplot2 to create a pie chart for your genotype data with custom colors:
  
  r
Kopiëren
Bewerken
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the data
genotype_data <- data.frame(
  Genotype = c("M1M1", "M1M2", "M1M3", "M1W", "M2M2", "M2W", "M3W", "WW"),
  Count = c(169, 5, 1, 250, 29, 45, 5, 486)
)

# Define the custom colors
custom_colors <- c("M1W" = "deeppink", "M1M1" = "red", "WW" = "grey",
                   "M2W" = "turquoise1", "M3W" = "khaki1", "M2M2" = "blue",
                   "M1M2" = "purple", "M1M3" = "orange")

# Create the pie chart
ggplot(genotype_data, aes(x = "", y = Count, fill = Genotype)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to pie chart
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme_void() +  # Remove background elements
  labs(title = "Genotype Distribution") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title
This script:
  
  Uses ggplot2 to create a bar chart and transform it into a pie chart with coord_polar().
Applies custom colors to each genotype.
Uses theme_void() to remove background elements for a clean pie chart look.
Run this in R and you should get a properly formatted pie chart. Let me know if you need modifications! 🚀







Je zei:
  this is my dataset, i want a pie chart of the column Genotype "data_staalname_sf
Simple feature collection with 990 features and 14 fields
Geometry type: POINT
Dimension:     XY
Bounding box:  xmin: 2.57393 ymin: 50.69342 xmax: 5.890181 ymax: 51.49893
Geodetic CRS:  WGS 84
First 10 features:
         ID Partner  Hok      Lat      Lon RATO_ID Datum Lat_Centr Lon_Centr Genotype ...11 ...12                  geometry Bekken jaar
1  RAT-0001 BRUSSEL BR67 50.83692 4.395306      NA    NA  50.83887  4.394882       WW    NA    NA POINT (4.395306 50.83692)     BR 2024
2  RAT-0005 BRUSSEL BR37 50.89174 4.319854      NA    NA  50.89174  4.319854      M2W    NA    NA POINT (4.319854 50.89174)     BR 2024
3  RAT-0006 BRUSSEL BR38 50.88144 4.326941      NA    NA  50.88144  4.326941      M2W    NA    NA POINT (4.326941 50.88144)     BR 2024
4  RAT-0008 BRUSSEL BR39 50.87114 4.334025      NA    NA  50.87114  4.334025      M2W    NA    NA POINT (4.334025 50.87114)     BR 2024
5  RAT-0009 BRUSSEL BR09 50.82678 4.306442      NA    NA  50.82678  4.306442       WW    NA    NA POINT (4.306442 50.82678)     BR 2024
6  RAT-0011 BRUSSEL BR18 50.84156 4.315631      NA    NA  50.84156  4.315631       WW    NA    NA POINT (4.315631 50.84156)     BR 2024
7  RAT-0012 BRUSSEL BR19 50.83126 4.322711      NA    NA  50.83126  4.322711       WW    NA    NA POINT (4.322711 50.83126)     BR 2024
8  RAT-0015 BRUSSEL BR08 50.83708 4.299359      NA    NA  50.83708  4.299359       WW    NA    NA POINT (4.299359 50.83708)     BR 2024
9  RAT-0019 BRUSSEL BR07 50.84738 4.292273      NA    NA  50.84738  4.292273       WW    NA    NA POINT (4.292273 50.84738)     BR 2024
10 RAT-0023 BRUSSEL BR17 50.85187 4.308547      NA    NA  50.85187  4.308547       WW    NA    NA POINT (4.308547 50.85187)     BR 2024"
ChatGPT zei:
  Here is an R script using ggplot2 to create a pie chart from the "Genotype" column in your dataset data_staalname_sf.

R Script:
  r
Kopiëren
Bewerken
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Count occurrences of each Genotype
genotype_counts <- data_staalname_sf %>%
  count(Genotype)

# Define custom colors for each genotype
custom_colors <- c("M1W" = "deeppink", "M1M1" = "red", "WW" = "grey",
                   "M2W" = "turquoise1", "M3W" = "khaki1", "M2M2" = "blue",
                   "M1M2" = "purple", "M1M3" = "orange")

# Create the pie chart
ggplot(genotype_counts, aes(x = "", y = n, fill = Genotype)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +  # Convert to a pie chart
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme_void() +  # Remove background elements
  labs(title = "Genotype Distribution") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Bar chart

# Count occurrences of each Genotype
genotype_counts <- data_staalname_sf %>%
  count(Genotype) %>%
  mutate(percentage = (n / sum(n)) * 100)  # Convert to percentages


# Create the horizontal bar chart
ggplot(genotype_counts, aes(x = 1, y = percentage, fill = Genotype)) +
  geom_bar(stat = "identity", width = 0.5) +  # Adjust bar width
  coord_flip() +  # Flip coordinates for horizontal bar
  scale_x_continuous(limits = c(0, 2)) +  # Set x-axis range (was y-axis)
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal() +  # Use a clean minimal theme
  labs(x = NULL, y = NULL, fill = NULL) +  # Remove axis labels and legend title
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverse legend order
  theme(legend.position = "none",  # Move legend to the bottom
        legend.text = element_text(size=12),
        axis.text = element_blank(),  # Remove axis text
        axis.ticks = element_blank(),
        panel.grid = element_blank(),  # Remove all gridlines
        panel.grid.major = element_blank(),  # Remove major gridlines (optional)
        panel.grid.minor = element_blank()   # Remove minor gridlines (optional)
  )  # Remove axis ticks

#table gentype per bekken

data_staalname_sf$Bekken<-substr(data_staalname_sf$Hok, 1, 2)

table_genotypes_bekken<-table(data_staalname_sf$Bekken, data_staalname_sf$Genotype)

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


## Visualizezations full sampling period
library(readxl)
data_staalname_sf$jaar<-rep(2024,length.out=nrow(data_staalname_sf))
data_staalname_sf<-data_staalname_sf[which(data_staalname_sf$Genotype!="NA"),]
data <- read_excel("data/2013 - 2019 BMK.xlsx", sheet = 1) 

# Remove the geometry column and save as a CSV
 <- st_drop_geometry(data_staalname_sf)

# Write the cleaned dataset to a CSV file
write.csv(data_staalname_sf_clean, "data_2024.csv", row.names = FALSE)


# Remove geometry from `data_staalname_sf` and select necessary columns
data_staalname_sf_selected <- data_staalname_sf %>%
  st_drop_geometry() %>%  # Drop the geometry column
  select(jaar, genotype = Genotype, Bekken)

# Select necessary columns from `data`
data_selected <- data %>%
  select(jaar, mutatie, Bekken)

# Standardize column names for consistency
data_staalname_sf_selected <- data_staalname_sf_selected %>%
  rename(mutatie = genotype)

# Combine both datasets
combined_data <- bind_rows(data_staalname_sf_selected, data_selected)

# Fix Demer
combined_data$Bekken[which(combined_data$Bekken=="DE")]<-"DM"
unique(combined_data$Bekken)

# Split alleles
combined_data_long <- combined_data %>%
  mutate(mutatie = gsub(" ", "", mutatie)) %>%  # Verwijder eventuele spaties
  separate(mutatie, into = c("allel_1", "allel_2"), sep = "(?<=W|M1|M2|M3)(?=W|M1|M2|M3)", remove = FALSE) %>%  # Splits bij W, M1, M2 of M3
  pivot_longer(cols = starts_with("allel_"), names_to = "allel_nummer", values_to = "allel") %>% 
  mutate(allel_nummer = ifelse(allel_nummer == "allel_1", 1, 2)) %>%  # Zet allel_1 en allel_2 om naar 1 en 2
  select(jaar, mutatie, Bekken, allel, allel_nummer)  # Behoud alleen relevante kolommen

# Calculate the proportion for each year
prop_M1 <- combined_data_long %>%
  group_by(jaar) %>%
  summarise(
    Proportion_M1 = sum(allel == "M1") / n()
  )

prop_M2 <- combined_data_long %>%
  group_by(jaar) %>%
  summarise(
    Proportion_M2 = sum(allel == "M2") / n()
  )

prop_M3 <- combined_data_long %>%
  group_by(jaar) %>%
  summarise(
    Proportion_M3 = sum(allel == "M3") / n()
  )

prop_R <- combined_data_long %>%
  group_by(jaar) %>%
  summarise(
    Proportion_W = sum(allel == "W") / n()
  )

prop_R$Proportion_R<- 1 - prop_R$Proportion_W

# Create the plot
ggplot() +
  geom_line(data = prop_M1, aes(x = jaar, y = Proportion_M1), color = "blue", size = 1) +
  geom_point(data = prop_M1, aes(x = jaar, y = Proportion_M1), color = "blue", size = 2) +
  
  geom_line(data = prop_M2, aes(x = jaar, y = Proportion_M2), color = "yellow", size = 1) +
  geom_point(data = prop_M2, aes(x = jaar, y = Proportion_M2), color = "yellow", size = 2) +
  
  geom_line(data = prop_M3, aes(x = jaar, y = Proportion_M3), color = "red", size = 1) +
  geom_point(data = prop_M3, aes(x = jaar, y = Proportion_M3), color = "red", size = 2) +
  
  geom_line(data = prop_R, aes(x = jaar, y = Proportion_R), color = "grey", size = 1) +
  geom_point(data = prop_R, aes(x = jaar, y = Proportion_R), color = "grey", size = 2) +
  
  labs(
    x = "Jaar",
    y = "Proportie"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )

# format for martijn
data_staalname_sf_clean

# Replace missing values in `exact_latitude` with corresponding values from `centroid_latitude`
data_staalname_sf_clean$Lat <- ifelse(is.na(data_staalname_sf_clean$Lat), 
                                      data_staalname_sf_clean$Lat_Centr, 
                                      data_staalname_sf_clean$Lat)

data_staalname_sf_clean$Lon <- ifelse(is.na(data_staalname_sf_clean$Lon), 
                                      data_staalname_sf_clean$Lon_Centr, 
                                      data_staalname_sf_clean$Lon)



write.csv2(data_staalname_sf_clean, "data_2024.csv", row.names = FALSE)

data_staalname_sf_clean <- st_as_sf(data_staalname_sf_clean, coords = c("Lon", "Lat"), crs = 4326)


data_staalname_sf_clean<- st_transform(test, crs = 31370)

data_staalname_sf_clean <- cbind(
  st_coordinates(data_staalname_sf_clean), 
  data_staalname_sf_clean
)

write.csv2(data_staalname_sf_clean, "data_2024.csv", row.names = FALSE)
