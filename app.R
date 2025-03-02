options(shiny.autoreload = TRUE)
# Install necessary packages if not installed
# install.packages(c("shiny", "leaflet", "sf", "tigris", "dplyr", "htmltools"))

library(shiny)
library(leaflet)
library(sf)
library(tigris)
library(dplyr)
library(htmltools)

# Load CSV Data
directory <- "data/"
birds_poultry <- read.csv(paste0(directory, "bird_flu_poultry.csv"))
birds_wildbirds <- read.csv(paste0(directory, "bird_flu_wildbirds.csv"))

# Get US county shapefile from `tigris`
counties <- counties(cb = TRUE, resolution = "20m") %>%
  rename(FIPS = GEOID)  # Match FIPS column for merging

# Ensure FIPS is a character for merging
birds_poultry$FIPS <- as.character(birds_poultry$FIPS)
birds_wildbirds$FIPS <- as.character(birds_wildbirds$FIPS)

# Rename columns for wild bird data
colnames(birds_wildbirds) <- c("FullGeoName.Wild", "FIPS", "State.Wild", "County.Wild", 
                               "Date.Detected.Wild", "Strain.Type.Source.Wild", "States.Wild", 
                               "Counties.Wild", "Totals.Wild")

# Merge data with county geometries
geo_data <- merge(counties, birds_poultry, by = "FIPS")
point_data <- merge(counties, birds_wildbirds, by = "FIPS")

# Transform object into the right format for R
geo_data <- st_transform(geo_data, crs = '+proj=longlat +datum=WGS84')

# Continuous color palette for poultry outbreaks
pal <- colorNumeric(palette = "viridis", domain = geo_data$Flock.Size)

# Popup information
p_popup <- paste0("<strong>Outbreak number: </strong>", geo_data$Outbreaks, 
                  "<br><strong>Flock Type: </strong>", geo_data$Flock.Type, 
                  "<br><strong>Outbreak Date: </strong>", geo_data$Outbreak.Date)

marker_popup <- paste0("<strong>Total Wild Birds Infected: </strong>", point_data$Totals.Wild, 
                       "<br><strong>Strain Type: </strong>", point_data$Strain.Type.Source.Wild, 
                       "<br><strong>Outbreak Date: </strong>", point_data$Date.Detected.Wild)

# Generating Centroids for wild bird locations
for (i in seq_len(nrow(point_data))) {
  centroid <- st_centroid(point_data[i, 21]) # Assuming geometry is at column 21
  if (i == 1) {
    points <- centroid
  } else {
    points <- rbind(points, centroid)
  }
}
lat_long <- as.data.frame(st_coordinates(points))
colnames(lat_long) <- c("long", "lat")

# Define UI
ui <- fluidPage(
  titlePanel("Flock Watch - 'Track the birds. Protect your herd.'"),
  leafletOutput("map", height = "700px")
)

# Define Server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addLegend("bottomleft", pal = pal, values = geo_data$Flock.Size, title = "Flock Size") %>% 
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("points", zIndex = 420) %>%
      addCircleMarkers(data = lat_long,
          group = "Wild Birds",
          options = pathOptions(pane = "points"),
          lat = lat_long$lat,
          lng = lat_long$long,
          radius = (point_data$Totals.Wild) / 50,
          popup = marker_popup) %>%
      addPolygons(weight = 1,
                  group = "Flocks",
                  options = pathOptions(pane = "polygons"),
                  data = geo_data,
                  color = pal(geo_data$Flock.Size),
                  stroke = 0.1,
                  opacity = 0.8,
                  popup = p_popup,
                  highlight = highlightOptions(weight = 2, color = "blue")) %>%
      addLayersControl(
        baseGroups = "Flocks",
        overlayGroups = "Wild Birds",
        options = layersControlOptions(collapsed = FALSE))
  })
}

# Run the app
shinyApp(ui, server)