options(shiny.autoreload = TRUE)
# Install and load necessary packages
packages <- c("shiny", "sf", "tigris", "dplyr", "leaflet", "htmltools", "shinyWidgets")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(packages, library, character.only = TRUE)

# Load CSV
directory <- "data/"
file_paths <- list(poultry = paste0(directory, "bird_flu_poultry.csv"),
                   wildbirds = paste0(directory, "bird_flu_wildbirds.csv"))
bird_data <- lapply(file_paths, read.csv)

# Get US county shapefile
counties <- counties(cb = TRUE, resolution = "20m") %>% rename(FIPS = GEOID)

# Prepare data
bird_data$poultry$FIPS <- as.character(bird_data$poultry$FIPS)
geo_data <- merge(counties, bird_data$poultry, by = "FIPS") %>%
  st_transform(crs = '+proj=longlat +datum=WGS84')

# Create a new Full Location column
geo_data$FullGeoName <- paste0(geo_data$STUSPS, ", ", geo_data$NAME)
geo_data$FullGeoNameAlt <- paste0(geo_data$NAME, ", ", geo_data$STUSPS)

# UI
title <- "FlockWatch"
ui <- fluidPage(
  titlePanel(title),
  sidebarLayout(
    sidebarPanel(
      textInput("location_input", "Enter Location (State, County or both)", ""),
      h4("Statistical Analysis"),
      verbatimTextOutput("correlation"),
      verbatimTextOutput("chi_square")
    ),
    mainPanel(leafletOutput("map"))
  )
)

# Server
server <- function(input, output, session) {
  pal <- colorNumeric(palette = "viridis", domain = geo_data$Flock.Size)
  
  filtered_data <- reactive({
    data <- geo_data
    
    if (input$location_input != "") {
      data <- data[grepl(input$location_input, data$FullGeoName, ignore.case = TRUE) |
                   grepl(input$location_input, data$FullGeoNameAlt, ignore.case = TRUE) |
                   grepl(input$location_input, data$NAME, ignore.case = TRUE) |
                   grepl(input$location_input, data$STUSPS, ignore.case = TRUE), ]
    }
    
    data
  })
  
  output$map <- renderLeaflet({
    leaflet(geo_data) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(data = geo_data, color = ~pal(Flock.Size), stroke = 0.1, opacity = 0.8,
                  popup = ~paste0("<strong>Outbreak number: </strong>", Outbreaks, 
                                 "<br><strong>Flock Type: </strong>", Flock.Type, 
                                 "<br><strong>Outbreak Date: </strong>", Outbreak.Date)) %>%
      addLegend('bottomleft', pal = pal, values = ~Flock.Size)
  })
  
  observe({
    data <- filtered_data()
    if (input$location_input != "") {
      if (nrow(data) > 0) {
        centroids <- st_point_on_surface(data)
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(data = geo_data, color = ~pal(Flock.Size), stroke = 0.1, opacity = 0.8,
                      popup = ~paste0("<strong>Outbreak number: </strong>", Outbreaks, 
                                     "<br><strong>Flock Type: </strong>", Flock.Type, 
                                     "<br><strong>Outbreak Date: </strong>", Outbreak.Date)) %>%
          flyTo(lng = mean(st_coordinates(centroids)[, 1]), 
                lat = mean(st_coordinates(centroids)[, 2]), zoom = 8)
      }
    }
  })
  
  output$correlation <- renderText({
    paste("Pearson correlation: ", round(cor(geo_data$Flock.Size, geo_data$Outbreaks, method = "pearson"), 3))
  })
  
  output$chi_square <- renderPrint({
    chisq.test(table(geo_data$Flock.Size, geo_data$Outbreaks), simulate.p.value = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
