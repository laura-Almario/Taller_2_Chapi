#limpiamos el entorno
rm(list = ls())

# Cargar paquetes antes de empezar el taller
install.packages('pacman')
install.packages('dplyr')
install.packages('sf')
install.packages('tidyr')
install.packages('rvest')
install.packages('tidyverse')
install.packages('writexl')
install.packages('rio')
install.packages('skimr')
install.packages('pastecs')
install.packages('PerformanceAnalytics')
install.packages('naniar')
install.packages('gtsummary')
install.packages('leaflet')
install.packages('tmaptools')
install.packages('osmdata')
install.packages('geojsonio')
install.packages("jqr")
install.packages("geojson")
install.packages("geojsonio")
install.packages(rgeos)
library()

#Fijamos el directorio
setwd("C:/Users/anama/OneDrive - Universidad de los andes/BIG DATA/TALLER 2")

#Llamamos las bases de datos
bd_test <- read.csv('test.csv')
bd_train <- read.csv('train.csv')

#Guardamos las bases de datos como rds
saveRDS(bd_test,"bd_test.rds")
saveRDS(bd_train,"bd_train.rds")

#Analisis inicial bases de datos
colnames(bd_train)
colnames(bd_test)
summary(bd_train)
summary(bd_test)

analisis_variables <- setdiff(names(bd_train), names(bd_test))

library(dplyr)
library(sf)
library(jqr)
library(geojson)
library(geojsonio)

#Unimos las bases para realizar los cambios
bd = bind_rows(bd_train,bd_test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)
head(bd)

#Como predictores de la descripción usamos parqueadero/garaje y si cuenta con estudio
bd$Parqueadero<- grepl("parqueadero(s)?", bd$title, ignore.case = TRUE) | grepl("parqueadero(s)?", bd$description, ignore.case = TRUE)

bd$Garaje<-grepl("garaje(s)?", bd$title, ignore.case = TRUE) | grepl("garaje(s)?", bd$description, ignore.case = TRUE)

bd$Parking <- ifelse(bd$Garaje == FALSE & bd$Parqueadero == FALSE, FALSE, TRUE)

bd$Estudio<-grepl("estudio(s)?", bd$title, ignore.case = TRUE) | grepl("estudio(s)?", bd$description, ignore.case = TRUE)

#Verificamos la bd
summary(bd)

sum(is.na(bd$bedrooms))
sum(is.na(bd$surface_total))
sum(is.na(bd$surface_covered))
sum(is.na(bd$bathrooms))
sum(is.na(bd$rooms))

#Cargamos los paquetes

library(osmdata)
library(dplyr)
library(sf)
library(leaflet)

#View(bd)
glimpse(bd)

#Como predictores externos usamos bares, estaciones de bus, restaurantes, parques, bancos y seguridad
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota",
                   featuretype = "boundary:administrative",
                   format_out = "sf_polygon") %>% .$multipolygon

bogota <- getbb(place_name = "Bogota",
                featuretype = "boundary:administrative",
                format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)

chapinero <- st_transform(chapinero,st_crs(bd))
bogota <- st_transform(bogota,st_crs(bd))

bd_chapinero <- bd[chapinero,]
head(bd_chapinero)

#Bares
available_features()
available_tags("amenity")
bar_chapinero <- opq(bbox = st_bbox(bd_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_chapinero , col="red", weight = 0.5, radius = 4)

available_tags("amenity")
bar_bogota <- opq(bbox = st_bbox(bd)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points

#Restaurantes

available_tags("amenity")
rest_chapinero <- opq(bbox = st_bbox(bd_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points 

leaflet() %>% addTiles() %>% addCircleMarkers(data=rest_chapinero , col="green", weight = 0.3, radius = 4)

available_tags("amenity")
rest_bogota <- opq(bbox = st_bbox(bd)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points 

#Parques

library(sf)
install.packages("rgeos")
library(rgeos)

available_tags("leisure")
park_chapinero <- opq(bbox = st_bbox(bd_chapinero)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons

available_tags("leisure")
park_bogota <- opq(bbox = st_bbox(bd)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons 

centroides_chapinero <- gCentroid(as(park_chapinero$geometry, "Spatial"), byid = T)
centroides_bogota <- gCentroid(as(park_bogota$geometry, "Spatial"), byid = T)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = park_chapinero, col = "blue",
              opacity = 0.8, popup = park_chapinero$name) %>%
  addCircles(lng = centroides_chapinero$x, 
             lat = centroides_chapinero$y, 
             col = "red", opacity = 1, radius = 1)


#Seguridad

available_tags("amenity")
seguridad_chapinero <- opq(bbox = st_bbox(bd_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf() %>% .$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=seguridad_chapinero , col="pink", weight = 0.5, radius = 4)

available_tags("amenity")
seguridad_bogota <- opq(bbox = st_bbox(bd)) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf() %>% .$osm_points 

#Estaciones de bus

available_tags("amenity")
bus_chapinero <- opq(bbox = st_bbox(bd_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf() %>% .$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_chapinero , col="black", weight = 0.3, radius = 4)

available_tags("amenity")
bus_bogota <- opq(bbox = st_bbox(bd)) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf() %>% .$osm_points

#Bancos

available_tags("amenity")
banco_chapinero <- opq(bbox = st_bbox(bd_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=banco_chapinero , col="purple")

available_tags("amenity")
banco_bogota <- opq(bbox = st_bbox(bd)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_point

#Distancia

#Distancia Parques

centroides_chapinero_sf <- st_as_sf(centroides_chapinero, coords = c("x", "y"))
dist_matrix <- st_distance(x = bd_chapinero, y = centroides_chapinero_sf)

centroides_bogota_sf <- st_as_sf(centroides_bogota, coords = c("x", "y"))
dist_matrix <- st_distance(x = bd, y = centroides_bogota_sf)

posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))
areas <- st_area(park_chapinero)

posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))
areas <- st_area(park_bogota)

#Distancia Bares

dist_bar_chapinero <- st_distance(x = bd_chapinero, y = bar_chapinero)
bd_chapinero$dist_bar_chapinero = apply(dist_bar_chapinero , 1 , min)

dist_bar_bogota <- st_distance(x = bd, y = bar_bogota)
bd$dist_bar_bogota = apply(dist_bar_bogota , 1 , min)

#Distancia Restaurantes

dist_rest_chapinero <- st_distance(x = bd_chapinero, y = rest_chapinero)
bd_chapinero$dist_rest_chapinero = apply(dist_rest_chapinero , 1 , min)

dist_rest_bogota <- st_distance(x = bd, y = rest_bogota)
bd$dist_rest_bogota = apply(dist_rest_bogota , 1 , min)

##Distancia Bancos

dist_banco_chapinero <- st_distance(x = bd_chapinero, y = banco_chapinero)
bd_chapinero$dist_banco_train = apply(dist_banco_chapinero , 1 , min)

dist_banco_bogota <- st_distance(x = bd, y = banco_bogota)
bd$dist_banco_bogota = apply(dist_banco_bogota , 1 , min)


#Distancia Seguridad

dist_seguridad_chapinero <- st_distance(x = bd_chapinero, y = seguridad_chapinero)
bd_chapinero$dist_seguridad_chapinero = apply(dist_seguridad_chapinero , 1 , min)

dist_seguridad_bogota <- st_distance(x = bd, y = seguridad_bogota)
bd$dist_seguridad_bogota = apply(dist_seguridad_bogota , 1 , min)

#Distancia Estaciones de Bus

dist_bus_chapinero <- st_distance(x = bd_chapinero, y = bus_chapinero)
bd_chapinero$dist_bus_chapinero = apply(dist_bus_chapinero , 1 , min)

dist_bus_bogota <- st_distance(x = bd, y = bus_bogota)
bd$dist_bus_bogota = apply(dist_bus_bogota , 1 , min)

#chapinero-distancia al parque El Virrey

parkch = getbb(place_name = "Parque El Virrey", 
               featuretype = "amenity",
               format_out = "sf_polygon")
parkch %>% head()

leaflet() %>% addTiles() %>% addPolygons(data=parkch , col="magenta")

dist_park_chapinero <- st_distance(x = bd_chapinero, y = parkch)
bd_chapinero$dist_park_chapinero = apply(dist_park_chapinero , 1 , min)

dist_park_bogota <- st_distance(x = bd, y = parkch)
bd$dist_park_bogota = apply(dist_park_bogota , 1 , min)

saveRDS(bd,"bd_variables.rds")