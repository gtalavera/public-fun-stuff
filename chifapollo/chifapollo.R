library(sf)
library(dplyr)
library(leaflet)

distritos <- st_read(dsn = "chifapollo/data/dist_lima.geojson")
provincias <- st_read(dsn = "chifapollo/data/prov_lima.geojson")
rest_amenities <- st_read(dsn = "chifapollo/data/rest_amenities.geojson")
rest_centroids <- st_read(dsn = "chifapollo/data/rests_centroids.geojson")

chifas1_a <- rest_amenities %>%
  filter(grepl('chifa|Chifa|CHIFA|chifa', name))
chifas1_b <- rest_amenities %>%
  filter(grepl('chifa|chinese', cuisine))
chifas1 <- union(chifas1_a, chifas1_b) %>% 
  select(full_id, osm_id, name, geometry)

chifas2_a <- rest_centroids %>%
  filter(grepl('chifa|Chifa|CHIFA|chifa', name))
chifas2_b <- rest_centroids %>%
  filter(grepl('chifa|chinese', cuisine))
chifas2 <- union(chifas2_a, chifas2_b)%>% 
  select(full_id, osm_id, name, geometry)

pollos1_a <- rest_amenities %>%
  filter(grepl('pollo|Pollo|pollos|Pollos|Pollería|Pollería', name))
pollos1_b <- rest_amenities %>%
  filter(grepl('chicken', cuisine))
pollos1 <- union(pollos1_a, pollos1_b)%>% 
  select(full_id, osm_id, name, geometry)

pollos2_a <- rest_centroids %>%
  filter(grepl('chifa|Chifa|CHIFA|chifa', name))
pollos2_b <- rest_centroids %>%
  filter(grepl('chifa|chinese', cuisine))
pollos2 <- union(pollos2_a, pollos2_b)%>% 
  select(full_id, osm_id, name, geometry)

chifas <- union(chifas1, chifas2)

mapa <- leaflet(distritos) %>% 
  addProviderTiles("OSM") %>%
  addmar()
  

