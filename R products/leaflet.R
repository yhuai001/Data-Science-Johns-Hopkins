library(leaflet)
my_map <- leaflet() %>% 
    addMarkers(lat = 39.298, lng = -76.589,
               popup = "Jeff Leek's Office")
my_map