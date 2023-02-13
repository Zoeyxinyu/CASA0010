library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(classInt)
library(rgeos)
library(rgdal)
library(tidyverse)
library(janitor)
library(geojsonR)


shop1 <- st_read("london shop(node).geojson") %>% 
  clean_names() %>% 
  select('id','name','addr_street','addr_postcode','shop','geometry') %>% 
  drop_na(name)

st_write(shop1,"shopdata1.geojson",driver = "GeoJSON")


shop2 <- st_read("convert-to-node.geojson") %>% 
  clean_names() %>%
  select('id','name','addr_street','addr_postcode','shop','geometry') %>% 
  drop_na(name)
  
st_write(shop2,"shopdata2.geojson",driver = "GeoJSON")






