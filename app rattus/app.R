
# rattus app - V1

library(shiny)
library(leaflet)
#library(googleway)
library(googlesheets4)
#library(googledrive)
library(data.table)
library(gargle)

# Enable writing to google sheet
gs4_auth(cache = ".secrets", email = "michiel.lathouwers@inbo.be")

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
      selectInput("num", "Aantal ratten:",
                  c(" " = NA,
                    "1" = "1",
                    "2-5" = "2-5",
                    "6-10" = "6-10",
                    "10+" = "10+")),
      checkboxGroupInput("action", "Actie (meerdere opties mogelijk):",
                         c("Mechanische bestrijding" = "mech",
                           "Preventie" = "prev",
                           "Rodenticide" = "rod")),
      conditionalPanel(
        condition = "input.action.includes('rod')",
        selectInput("rod", "Type rodenticide (indien van toepassing):",
                    c(" " = NA,
                      "Difenacoum 0,005%" = "dif_5",
                      "Difenacoum 0,0025%" = "dif_25",
                      "Bromadialone 0,005%" = "bro_5",
                      "Bromadialone 0,0025%" = "bro_25",
                      "Cholecalciferol" = "chol",
                      "Ander" = "ander")),
        numericInput("amount", "Hoeveelheid (g):", 0, min = 1, max = 1000),
        checkboxInput("public", "Openbaar domein")
      ),
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
      num<-input$num,
      action<-input$action,
      rod<-input$rod,
      amount<-input$amount,
      public<-input$public,
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
