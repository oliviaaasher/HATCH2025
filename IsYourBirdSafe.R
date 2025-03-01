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

# UI
title <- "Bird Flu Outbreaks in Poultry"
ui <- fluidPage(
  titlePanel(title),
  sidebarLayout(
    sidebarPanel(
      h4("Statistical Analysis"),
      selectInput("state", "Select State:", choices = unique(geo_data$STATE_NAME), selected = "Georgia"),
      uiOutput("county_select"),
      verbatimTextOutput("correlation"),
      verbatimTextOutput("chi_square")
    ),
    mainPanel(leafletOutput("map"))
  )
)

# Server
server <- function(input, output, session) {
  pal <- colorNumeric(palette = "viridis", domain = geo_data$Flock.Size)
  
  # Update county choices based on selected state
  output$county_select <- renderUI({
    req(input$state)
    counties_filtered <- geo_data %>% filter(STATE_NAME == input$state)
    selectInput("county", "Select County:", choices = unique(counties_filtered$NAME), selected = unique(counties_filtered$NAME)[1])
  })
  
  # Reactive data filtered by selected state and county
  filtered_data <- reactive({
    req(input$state, input$county)
    geo_data %>% filter(STATE_NAME == input$state, NAME == input$county)
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = mean(st_coordinates(st_centroid(data))[,1]), 
              lat = mean(st_coordinates(st_centroid(data))[,2]), zoom = 8) %>%
      addPolygons(color = ~pal(Flock.Size), stroke = 0.1, opacity = 0.8,
                  popup = ~paste0("<strong>Outbreak number: </strong>", Outbreaks, 
                                 "<br><strong>Flock Type: </strong>", Flock.Type, 
                                 "<br><strong>Outbreak Date: </strong>", Outbreak.Date)) %>%
      addLegend('bottomleft', pal = pal, values = ~Flock.Size)
  })
  
  output$correlation <- renderText({
    data <- filtered_data()
    paste("Pearson correlation: ", round(cor(data$Flock.Size, data$Outbreaks, method = "pearson"), 3))
  })
  
  output$chi_square <- renderPrint({
    data <- filtered_data()
    chisq.test(table(data$Flock.Size, data$Outbreaks))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)