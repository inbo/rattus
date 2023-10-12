# Rattenmonitortool - Draft script

getwd()
library(tidyverse)

# Test visualisatie (Data VMM)

# Read data
VMM<-read.csv('./VMM_Data_Test.csv',sep=';')
VMM$Gif<-VMM$Registratie.Hoeveelheid..gr.
VMM$Gif<-as.numeric(VMM$Gif)

# Data wrangling
Data_Monitor<-aggregate(Gif ~ Gemeente + Maand , data = VMM, sum)

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


# Plot number of locations and poison per month per municipality
library(sf)

Provincies<-st_read('./Provincies_shapefile/BELGIUM_-_Provinces.shp')
Limburg<-subset(Provincies,Provincies$NAME_2=="Limburg")
ggplot()+ geom_sf(data=Limburg)

Data_Monitor_sf <- st_as_sf(Data_Monitor, coords = c("Lon", "Lat"), 
                   crs = "WGS 84", agr = "constant")
st_crs(Data_Monitor_sf)<-st_crs(Limburg)

ggplot(Data_Monitor_sf[which(Data_Monitor_sf$Maand==1),]) + 
  geom_sf(data=Limburg) +
  geom_sf(aes(size = Locaties, fill = Gif), shape = 21, alpha = 0.7) +
  scale_fill_viridis_c(limits=c(100,27600),breaks=c(0,5000,10000,15000,20000,25000)) +
  scale_size_continuous(limits = c(0, 90),breaks=c(0,30,60,90))

ggplot(Data_Monitor_sf[which(Data_Monitor_sf$Maand==9),]) + 
  geom_sf(data=Limburg) +
  geom_sf(aes(size = Locaties, fill = Gif), shape = 21, alpha = 0.7) +
  scale_fill_viridis_c(limits=c(100,27600),breaks=c(0,5000,10000,15000,20000,25000)) +
  scale_size_continuous(limits = c(1, 90),breaks=c(0,30,60,90))


###################

# Test App

library(shiny)
library(leaflet)
#library(googleway)
library(googlesheets4)
#library(googledrive)
library(data.table)

# Enable writing to google sheet

gs4_auth()

file_url<-"https://docs.google.com/spreadsheets/d/1vZ3F1sLUqvvIwrB5varsrT7y5fJbj_BR_TEuthay5PE/edit#gid=0"
sheet_id<-as_sheets_id(file_url)

# App user interface

ui <- fluidPage(
  # Application title
  h1("Meldpunt Bruine Rat Bestrijding", align = "center"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h4("Nieuwe Melding"),
      dateInput("date", "Datum:", format = "mm/dd/yy"),
      h5(strong("Locatie (klik op de kaart):")),
      numericInput("lon", "Lengtegraad", 0, min = 1, max = 100),
      numericInput("lat", "Breedtegraad", 0, min = 1, max = 100),
      checkboxGroupInput("action", "Actie (meerdere opties mogelijk):",
                         c("Mechanische bestrijding" = "mech",
                           "Preventie" = "prev",
                           "Rodenticide" = "rod")),
      selectInput("rod", "Type rodenticide (indien van toepassing):",
                  c(" " = NA,
                    "Difenacoum 0,005%" = "dif_5",
                    "Difenacoum 0,0025%" = "dif_25",
                    "Bromadialone 0,005%" = "bro_5",
                    "Bromadialone 0,0025%" = "bro_25",
                    "Cholecalciferol" = "chol",
                    "Ander" = "ander")),
      numericInput("amount", "Hoeveelheid (g):", 0, min = 1, max = 1000),
      textInput("comment", "Opmerking:", value = ""),
      actionButton("submitbutton", "Indienen")
    ),
    mainPanel(
      leafletOutput("map"),
      uiOutput("thankyoutext")  # Added uiOutput element for displaying the message
    )
  )
)


# Server (with writing functionality)

server <- function(input, output, session) {
  # Create a reactiveVal to store the marker data
  marker_data <- reactiveVal(data.frame(lon = numeric(0), lat = numeric(0)))
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0)) %>%
      addTiles() %>%
      addMarkers(data = marker_data()) %>%
      # Set the initial view to Flanders
      setView(lng = 4.469936, lat = 51.065874, zoom = 8)
  })
  
  # Observe the click event on the map and update the marker_data
  observeEvent(input$map_click, {
    new_marker <- data.frame(lon = input$map_click$lng, lat = input$map_click$lat)
    
    # Clear previous marker data and add the new marker data
    marker_data(data.frame(lon = numeric(0), lat = numeric(0)))
    marker_data(rbind(marker_data(), new_marker))
    
    # Update the numericInput fields
    updateNumericInput(session, "lon", value = new_marker$lon)
    updateNumericInput(session, "lat", value = new_marker$lat)
  })
  
  to_be_done_at_submit <- eventReactive(input$submitbutton, {
    #Collect data
    dtData <- data.table(
      date <- input$date,
      lon<-input$lon,
      lat<-input$lat,
      action<-input$action,
      rod<-input$rod,
      amount<-input$amount,
      comment<-input$comment
    )
    
    #Put data on drive
    sheet_append(ss = sheet_id, 
                 data = dtData,
                 sheet = "Test")
    #Say thankyou
    h5("Melding geregistreerd. Bekijk alle meldingen",
       a("hier", 
         href = "https://docs.google.com/spreadsheets/d/1vZ3F1sLUqvvIwrB5varsrT7y5fJbj_BR_TEuthay5PE/edit#gid=0")
    )
  })
  output$thankyoutext <- renderUI({
    to_be_done_at_submit()
  })

}


# Run app

shinyApp(ui, server)


# TO DO: meerdere actions geeft meerdere rijen, werken met ID? Of verschillende acties in verschillende kolommen?
#        zoom in van map wanneer clicken



# visualisatie op basis van google sheet

bruine_rat <- read_sheet("https://docs.google.com/spreadsheets/d/1vZ3F1sLUqvvIwrB5varsrT7y5fJbj_BR_TEuthay5PE/edit#gid=0")

ui <- fluidPage(
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(bruine_rat$lon,bruine_rat$lat)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles(
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)











