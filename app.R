library(readr)
library(stringr)

library(tidyverse)
library(leaflet)
library(shiny)

library(pubtheme)

library(geojsonsf)
library(sf)

FILE_NAMES <- c("data/earlyon_centres.rds", "data/green_spaces.rds")

if (all(file.exists(FILE_NAMES))) {
  earlyon_centres <- readRDS(FILE_NAMES[1])
  green_spaces <- readRDS(FILE_NAMES[2])
} else {
  # Load data
  earlyon_centres <- st_read("data/EarlyON Child and Family Centres Locations - geometry.json")
  green_spaces <- st_read("data/Green Spaces - 4326.geojson")
  
  # Process data
  green_spaces <- green_spaces %>%
    mutate(AREA_NAME = str_to_title(AREA_NAME)) %>%
    filter(!(AREA_CLASS %in% c("Cemetery",
                               "Cul de Sac",
                               "Golf Course",
                               "Field/Utility Corridor",
                               "Orphaned Space",
                               "Traffic Island",
                               "OTHER_CEMETERY",
                               "OTHER_CITY",
                               "OTHER_GOLFCOURSE",
                               "OTHER_HYDRO",
                               "OTHER_PROVINCIAL_FEDERAL",
                               "OTHER_ROAD",
                               "OTHER_TRCA",
                               "OTHER_UNKNOWN"
    )))
  
  # Ensure same coordinate reference system
  CRS_new <- CRS("epsg:4326")
  
  earlyon_centres <- st_as_sf(earlyon_centres, crs = CRS_new)
  earlyon_centres <- st_transform(earlyon_centres, crs = CRS_new)
  
  green_spaces <- st_as_sf(green_spaces, crs = CRS_new)
  green_spaces <- st_transform(green_spaces, crs = CRS_new)
  
  # Find each centre's distance to closest green space
  distances <- st_distance(earlyon_centres, green_spaces)
  earlyon_centres$distance <- apply(distances, 1, min)
  earlyon_centres$distance <- round(earlyon_centres$distance)
  
  # Save
  saveRDS(earlyon_centres, "data/earlyon_centres.rds")
  saveRDS(green_spaces, "data/green_spaces.rds")
}

# Shiny app UI
ui <- fluidPage(
  titlePanel("Green spaces and early childhood centres in Toronto"),
  
  sidebarLayout(
    sidebarPanel(
      p(strong("Early childhood centre proximity to green space")),
      p("Click on map markers of centres and spaces for more information. 
        Filter by distance to display all centres within some distance of
        a green space."),
      p("Data from the",
        a("City of Toronto Open Data Portal.",
          href = "https://open.toronto.ca/catalogue/")),
      br(),
      
      sliderInput("distance_slider", "Distance to closest green space (m)",
                  min = min(earlyon_centres$distance),
                  max = max(earlyon_centres$distance),
                  value = 50),
      
      textOutput("distance_text")
    ),
    
    mainPanel(
      leafletOutput("map", height = "500px", width = "100%"),
      
      br(),
      
      plotOutput("graph")
    )
  )
)

# Shiny app server
server <- function(input, output) {
  # Filter data
  filtered_earlyon_centres <- reactive({
    earlyon_centres %>%
      filter(distance <= input$distance_slider)
  })
  
  # Text for how many centres/what percentage of centres are displayed
  output$distance_text <- renderText({
    paste(paste("Showing",
                nrow(filtered_earlyon_centres()),
                "of",
                nrow(earlyon_centres),
                "early childhood centres within",
                input$distance_slider,
                "m of a green space, "),
          paste0(round(nrow(filtered_earlyon_centres()) / nrow(earlyon_centres) * 100, 2),
                 "% of all centres"))
  })
  
  # Map of green spaces and early childhood centres
  output$map <- renderLeaflet({
    leaflet(green_spaces) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(popup = green_spaces$AREA_NAME,
                  color = "green",
                  fillOpacity = 0.5,
                  weight = 1) %>%
      addCircleMarkers(lat = filtered_earlyon_centres()$lat,
                       lng = filtered_earlyon_centres()$lng,
                       popup = paste(filtered_earlyon_centres()$program_name,
                                     filtered_earlyon_centres()$address,
                                     paste(filtered_earlyon_centres()$distance,
                                           "m from green space"),
                                     sep = "<br>"),
                       radius = 5,
                       weight = 2,
                       color = "red") %>%
      setView(lng = -79.37, lat = 43.72, zoom = 11)
  })
  
  # Histogram how many centres are within some distance of green space
  output$graph <- renderPlot({
    ggplot(earlyon_centres,
           aes(x = distance)) +
      geom_histogram(binwidth = 20) +
      labs(title = "Distribution of early childhood centre proximity to green space",
           x = "Distance to closest green space (m)",
           y = "Number of centres") +
      theme_pub("hist")
  })
}

# Run the app
shinyApp(ui, server)