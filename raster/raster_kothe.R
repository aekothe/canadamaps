
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

#elevation
wmap <- readOGR("canvec_1M_CA_Hydro_shp/canvec_1M_CA_Hydro/watercourse_1.shp")
wmap <- spTransform(wmap, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
wmap.df <- fortify(wmap)

#map
ggplot(wmap.df, aes(long, lat, group = group)) +
  geom_polygon(color = "blue", fill = "white", size = .3) +
  theme_map()
