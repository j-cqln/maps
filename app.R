library(readr)
library(stringr)

library(tidyverse)
library(leaflet)
library(shiny)

library(pubtheme)

library(geojsonsf)
library(sf)

FILE_NAMES <- c("data/earlyon_centres.rds",
                "data/green_spaces.rds",
                "data/neighbourhoods.rds")

if (all(file.exists(FILE_NAMES))) {
  earlyon_centres <- readRDS(FILE_NAMES[1])
  green_spaces <- readRDS(FILE_NAMES[2])
  neighbourhoods <- readRDS(FILE_NAMES[3])
} else {
  read_excel_all_sheets <- function(filename, tibble = TRUE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
  }
  
  # Load data
  earlyon_centres <- st_read("data/EarlyON Child and Family Centres Locations - geometry.json")
  green_spaces <- st_read("data/Green Spaces - 4326.geojson")
  neighbourhoods <- st_read("data/Neighbourhoods.geojson")
  neighbourhoods_profiles <- read_excel("data/neighbourhood-profiles-2021-158-model.xlsx")
  neighbourhoods_environment <- read_excel_all_sheets("data/wellbeing-toronto-environment.xlsx")
  
  # Process public spaces
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
    ))) %>%
    rename(neighbourhood = AREA_NAME) %>%
    select(neighbourhood, geometry)
  
  earlyon_centres <- earlyon_centres %>%
    select(program_name, address, lat, lng, geometry)
  
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
  
  # Process neighbourhoods
  neighbourhoods <- st_as_sf(neighbourhoods, crs = CRS_new)
  neighbourhoods <- st_transform(neighbourhoods, crs = CRS_new)
  
  neighbourhoods_profiles <- neighbourhoods_profiles %>%
    rename(information = "Neighbourhood Name") %>%
    filter(information == "Median total income in 2020 ($)") %>%
    pivot_longer(cols = -information, names_to = "neighbourhood", values_to = "income") %>%
    mutate(neighbourhood = recode(neighbourhood,
                                  "East End Danforth" = "East End-Danforth",
                                  "Church-Wellesley" = "Church-Yonge Corridor",
                                  "Fenside-Parkwoods" = "Parkwoods",
                                  "Junction-Wallace Emerson" = "Wallace Emerson-Junction",
                                  "East L'Amoreaux" = "L'Amoreaux East",
                                  "O`Connor Parkview" = "O'Connor-Parkview",
                                  "Taylor Massey" = "Taylor-Massey",
                                  "Yonge-St.Clair" = "Yonge-St. Clair",
                                  .default = neighbourhood)) %>%
    mutate(income = as.numeric(income)) %>%
    select(neighbourhood, income)
  
  neighbourhoods_environment <- neighbourhoods_environment$`RawData-Ref Period 2011` %>%
    rename(neighbourhood = Neighbourhood,
           green_spaces = `Green Spaces`) %>%
    mutate(neighbourhood = recode(neighbourhood,
                                  "Cabbagetown-South St.James Town" = "Cabbagetown-South St. James Town",
                                  "Dovercourt-Wallace Emerson-Juncti" = "Wallace Emerson-Junction",
                                  "Islington-City Centre West" = "Islington",
                                  "L'Amoreaux" = "L'Amoreaux West",
                                  "Malvern" = "Malvern West",
                                  "Mimico" = "Mimico-Queensway",
                                  "North St.James Town" = "North St. James Town",
                                  "O`Connor Parkview" = "O'Connor-Parkview",
                                  "Parkwoods-Donalda" = "Parkwoods",
                                  "Waterfront Communities-The Island" = "St Lawrence-East Bayfront-The Islands",
                                  "Weston-Pellam Park" = "Weston-Pelham Park",
                                  "Yonge-St.Clair" = "Yonge-St. Clair",
                                  .default = neighbourhood)) %>%
    mutate(green_spaces = ifelse(is.na(green_spaces), 0, green_spaces))
  
  neighbourhoods_environment <- neighbourhoods_environment %>%
    add_row(neighbourhood = "Malvern East", green_spaces = neighbourhoods_environment$green_spaces[neighbourhoods_environment$neighbourhood == "Malvern West"]) %>%
    add_row(neighbourhood = "L'Amoreaux East", green_spaces = neighbourhoods_environment$green_spaces[neighbourhoods_environment$neighbourhood == "L'Amoreaux West"]) %>%
    add_row(neighbourhood = "Dovercourt", green_spaces = neighbourhoods_environment$green_spaces[neighbourhoods_environment$neighbourhood == "Wallace Emerson-Junction"]) %>%
    select(neighbourhood, green_spaces)
  
  neighbourhoods <- neighbourhoods %>%
    mutate(neighbourhood = recode(AREA_NAME,
                                  "Cabbagetown-South St.James Town" = "Cabbagetown-South St. James Town",
                                  "Church-Wellesley" = "Church-Yonge Corridor",
                                  "Danforth East York" = "Danforth-East York",
                                  "Fenside-Parkwoods" = "Parkwoods",
                                  "Junction-Wallace Emerson" = "Wallace Emerson-Junction",
                                  "East L'Amoreaux" = "L'Amoreaux East",
                                  "North St.James Town" = "North St. James Town",
                                  "O`Connor Parkview" = "O'Connor-Parkview",
                                  "Yonge-St.Clair" = "Yonge-St. Clair",
                                  .default = AREA_NAME)) %>%
    st_transform(crs = CRS_new) %>%
    full_join(neighbourhoods_profiles, by = "neighbourhood") %>%
    st_transform(crs = CRS_new) %>%
    full_join(neighbourhoods_environment, by = "neighbourhood") %>%
    filter(!st_is_empty(geometry)) %>%
    select(neighbourhood, geometry, income, green_spaces)
  
  # Ensure same coordinate system
  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Create combined neighbourhood data set
  x_df <- earlyon_centres
  x_df$geometry <- NULL
  x_df <- as.data.frame(x_df)
  x_data <- sf::as_Spatial(st_geometry(earlyon_centres), IDs = as.character(1:nrow(earlyon_centres)))
  x_data <- sp::SpatialPoints(x_data, proj4string = proj4string)
  x <- sp::SpatialPointsDataFrame(x_data, data = x_df)
  
  y_df <- neighbourhoods
  y_df$geometry <- NULL
  y_df <- as.data.frame(y_df)
  y_data <- sf::as_Spatial(st_geometry(neighbourhoods), IDs = as.character(1:nrow(neighbourhoods)))
  y <- sp::SpatialPolygonsDataFrame(y_data, data = y_df)
  
  counts <- sp::over(x, y) %>%
    group_by(neighbourhood) %>%
    summarise(earlyon_centres_count = n())
  
  neighbourhoods <- neighbourhoods %>%
    st_transform(crs = CRS_new) %>%
    full_join(counts, by = "neighbourhood") %>%
    mutate(earlyon_centres_count = ifelse(is.na(earlyon_centres_count), 0, earlyon_centres_count))
  
  # Save
  saveRDS(earlyon_centres, "data/earlyon_centres.rds")
  saveRDS(green_spaces, "data/green_spaces.rds")
  saveRDS(neighbourhoods, "data/neighbourhoods.rds")
}

# Shiny app UI
ui <- fluidPage(
  titlePanel("Green spaces, early childhood centres, income in Toronto"),
  
  tabsetPanel(
    tabPanel("Green spaces and early childhood centres",
             fluid = TRUE,
             
             br(),
             
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
                 
                 textOutput("distance_text")),
               
               mainPanel(
                 leafletOutput("map_centres_green_spaces", height = "500px", width = "100%"),
                 br(),
                 plotOutput("graph")
               )
             )
    ),
    
    tabPanel("Income, centres, green spaces by neighbourhood",
             fluid = TRUE,
             
             br(),
             
             sidebarLayout(
               sidebarPanel(
                 p(strong("Neighbourhood-level information")),
                 p("Click on neighbourhoods for more information.
                   Due to naming differences, some neighbourhoods
                   do not have all data"),
                 p("Data from the",
                   a("City of Toronto Open Data Portal.",
                     href = "https://open.toronto.ca/catalogue/")),
                 br()),
               
               mainPanel(
                 leafletOutput("map_income", height = "500px", width = "100%"),
                 br(),
                 leafletOutput("map_green_spaces", height = "500px", width = "100%"),
                 br(),
                 leafletOutput("map_centres", height = "500px", width = "100%")
               )
             )
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
  
  # Map of income per neighbourhood
  output$map_income <- renderLeaflet({
    pal <- colorNumeric(
      palette = "Reds",
      domain = neighbourhoods$income)
    
    leaflet(neighbourhoods) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(popup = paste0(neighbourhoods$neighbourhood, "<br>",
                                 "Income: CAD $", neighbourhoods$income),
                  fillOpacity = 1,
                  color = ~pal(income),
                  weight = 1) %>%
      addLegend(pal = pal,
                values = ~income,
                opacity = 1,
                title = NULL,
                position = "bottomright") %>%
      setView(lng = -79.37, lat = 43.72, zoom = 11)
  })
  
  # Map of green space area per neighbourhood
  output$map_green_spaces <- renderLeaflet({
    pal <- colorNumeric(
      palette = "Greens",
      domain = neighbourhoods$green_spaces)
    
    leaflet(neighbourhoods) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(popup = paste0(neighbourhoods$neighbourhood, "<br>",
                                 "Green space (sq km):", neighbourhoods$green_spaces),
                  fillOpacity = 1,
                  color = ~pal(green_spaces),
                  weight = 1) %>%
      addLegend(pal = pal,
                values = ~green_spaces,
                opacity = 1,
                title = NULL,
                position = "bottomright") %>%
      setView(lng = -79.37, lat = 43.72, zoom = 11)
  })
  
  # Map of centres per neighbourhood
  output$map_centres <- renderLeaflet({
    pal <- colorNumeric(
      palette = "Blues",
      domain = neighbourhoods$earlyon_centres_count)
    
    leaflet(neighbourhoods) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(popup = paste0(neighbourhoods$neighbourhood, "<br>",
                                 neighbourhoods$earlyon_centres_count,
                                 " centres"),
                  fillOpacity = 1,
                  color = ~pal(earlyon_centres_count),
                  weight = 1) %>%
      addLegend(pal = pal,
                values = ~earlyon_centres_count,
                opacity = 1,
                title = "Centres in neighbourhood",
                position = "bottomright") %>%
      setView(lng = -79.37, lat = 43.72, zoom = 11)
  })
  
  # Map of green spaces and early childhood centres
  output$map_centres_green_spaces <- renderLeaflet({
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
                       color = "blue") %>%
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