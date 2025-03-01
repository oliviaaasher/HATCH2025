# Install Libraries
library(sf)         # For spatial data handling
library(tigris)     # For getting county boundaries
library(dplyr)      # For data manipulation
library(leaflet)  #interactive maps
library(htmltools) # Add html popups

# Load CSV
directory <- "data/"
birds_poultry <- read.csv(paste0(directory, "bird_flu_poultry.csv"))

birds_wildbirds <- read.csv(paste0(directory, "bird_flu_wildbirds.csv"))

# Get US county shapefile from `tigris`
counties <- counties(cb = TRUE, resolution = "20m") %>%
  rename(FIPS = GEOID)  # Match FIPS column for merging

# Ensure FIPS is a character for merging
birds_poultry$FIPS <- as.character(birds_poultry$FIPS)

# Merge your data with county geometries
geo_data <- merge(counties, birds_poultry, by = "FIPS")

geo_data <- st_transform(geo_data, crs = '+proj=longlat +datum=WGS84')

# Continuous palette
pal <- colorNumeric(palette = "viridis", domain = geo_data$Flock.Size)
p_popup <- paste0("<strong>Outbreak number: </strong>", 
                  geo_data$Outbreaks, 
                  "<br><strong>Flock Type: </strong>", 
                  geo_data$Flock.Type, 
                  "<br><strong>Outbreak Date: </strong>",
                  geo_data$Outbreak.Date)

map <- leaflet() %>%
       addTiles() %>%
       setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
       addPolygons(data = geo_data, color = pal(geo_data$Flock.Size), stroke = 0.1, opacity = 0.8, popup = p_popup)

map %>% addLegend('bottomleft', pal = pal, values = geo_data$Flock.Size)
map 



# Statistical Analysis ----
# Calculate Pearson correlation between Flock Size and Outbreak
pearson_correlation <- cor(geo_data$Flock.Size, geo_data$Outbreaks, method = "pearson")

contingency_table <- table(geo_data$Flock.Size, geo_data$Outbreaks)

# Perform Chi-Square test
chi_square_test <- chisq.test(contingency_table)