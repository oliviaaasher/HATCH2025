# Install Libraries
library(sf)         # For spatial data handling
library(tigris)     # For getting county boundaries
library(dplyr)      # For data manipulation
library(leaflet)  #interactive maps
library(htmltools) # Add html popups

# Load CSV
directory <- "/Users/oliviaasher/Library/Mobile Documents/com~apple~CloudDocs/bird_flu"
birds_poultry <- read.csv(paste0(directory, "/bird_flu_poultry.csv"))
birds_wildbirds <- read.csv(paste0(directory, "/bird_flu_wildbirds.csv"))

# Get US county shapefile from `tigris`
counties <- counties(cb = TRUE, resolution = "20m") %>%
  rename(FIPS = GEOID)  # Match FIPS column for merging

# Ensure FIPS is a character for merging
birds_poultry$FIPS <- as.character(birds_poultry$FIPS)
birds_wildbirds$FIPS <- as.character(birds_wildbirds$FIPS)

colnames(birds_wildbirds) <- c("FullGeoName.Wild", "FIPS", "State.Wild", "County.Wild", "Date.Detected.Wild", "Strain.Type.Source.Wild", "States.Wild", "Counties.Wild", "Totals.Wild")

# Merge your data with county geometries
geo_data <- merge(counties, birds_poultry, by = "FIPS")

geo_data <- st_transform(geo_data, crs = '+proj=longlat +datum=WGS84')

# Continuous palette
pal <- colorNumeric(palette = "viridis", domain = geo_data$Flock.Size)
p_popup <- paste0("<strong>Outbreak number: </strong>", geo_data$Outbreaks, "<br><strong>Flock Type: </strong>", geo_data$Flock.Type, "<br><strong>Outbreak Date: </strong>",geo_data$Outbreak.Date)

map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
  addPolygons(weight = 1,
              data = geo_data,
              color = pal(geo_data$Flock.Size),
              stroke = 0.1,
              opacity = 0.8,
              popup = p_popup,
              highlight = highlightOptions(weight = 2,
                                           color = "blue",
                                           bringToFront = TRUE)) %>%
  addLegend("bottomleft", pal = pal, values = geo_data$Flock.Size)


