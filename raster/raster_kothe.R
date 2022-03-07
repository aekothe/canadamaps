
library(maptools)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)
library(socviz)
library(scales)
library(cowplot)
library(ggmap)
library(dplyr)
library(showtext)
library(sf)
library(extrafont)
library(ggplot2)
library(raster)

setwd("/Users/annkothe/Documents/GitHub/canadamaps/raster")

#base provinces
bmap <- readOGR("lpr_000b21a_e/lpr_000b21a_e.shp")
map.df <- fortify(map, region = "PRUID")

#water
wmap <- readOGR("ghy_000c06a_e/ghy_000c06a_e.shp")
wmap.df <- fortify(wmap)

#map
ggplot(wmap.df, aes(long, lat, group = group)) +
  geom_polygon(color = "blue", size = .3) +
  coord_map(projection = "albers",  lat0 = 49, lat1 = 75) +
  theme_map()
