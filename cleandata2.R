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

shop <- st_read("mergedfile.geojson") %>% 
  filter(`shop`!= "yes")
st_write(shop,"mergedfile1.geojson",driver = "GeoJSON")

shopcsv <- read_csv("mergedfile1.csv") %>% 
  clean_names() %>% 
  mutate(coordinate=str_replace_all(wkt,pattern = c("POINT "=""))) %>% 
  mutate(coordinate=str_replace_all(coordinate,pattern = c(" "=","))) %>% 
  mutate(lat_lon=str_sub(coordinate, 2,-2)) %>% 
  mutate(separate(.,col = lat_lon,into = c("lat","lon"),sep = ","))
st_write(shopcsv,"LondonShop.csv",driver="csv")

coor <- shopcsv %>% 
  filter(is.na(addr_street)) %>% 
  select("lat","lon")
st_write(coor,"lat-lon.csv",driver="csv")
