#Canada OMS Project
#Angela Kothe
#04.28.2021

library(remotes)
library(sf)
remotes::install_github("ropensci/osmdata")
library(tidyverse)
library(osmdata)
library(showtext)
library(ggmap)

#toronto data
toronto <- getbb("Toronto Canada")

toroads <- toronto%>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary", "motorway_link", "trunk_link", "primary_link")) %>%
  osmdata_sf()

toroads2 <- toronto%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


toroads3 <- toronto%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

tolake <- toronto%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "lake")) %>%
  osmdata_sf()

tocoast <- toronto%>%
  opq()%>%
  add_osm_feature(key = "natural", value="coastline") %>%
  osmdata_sf()

towaters <- toronto%>%
  opq()%>%
  add_osm_feature(key = "natural", value="water") %>%
  osmdata_sf()

totrain <- toronto%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

#base map toronto
font_add_google(name = "Lato", family = "lato") 
showtext_auto()
ggplot() +
  geom_sf(data = tocoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = tocoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = towaters$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = towaters$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = tolake$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = tolake$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = totrain$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = toroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = toroads2$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = toroads3$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = toronto[1,], 
           ylim = toronto[2,],
           expand = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "TORONTO", subtitle = "43.58025° N/ 79.63927° W")

#alcohol map
liquor <- toronto%>%
  opq()%>%
  add_osm_feature(key = "shop", value=c("alcohol")) %>%
  osmdata_sf()

liquor

bar <- toronto%>%
  opq()%>%
  add_osm_feature(key = "amenity", value=c("bar")) %>%
  osmdata_sf()

bar

pub <- toronto%>%
  opq()%>%
  add_osm_feature(key = "amenity", value=c("pub")) %>%
  osmdata_sf()

pub

breweries <- toronto%>%
  opq()%>%
  add_osm_feature(key = "craft", value=c("brewery")) %>%
  osmdata_sf()

breweries

font_add_google(name = "Lato", family = "lato") 
showtext_auto()
ggplot() +
  geom_sf(data = tocoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = tocoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1)  +
  geom_sf(data = towaters$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = towaters$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = tolake$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = tolake$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = totrain$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = toroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = toroads2$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = toroads3$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = bar$osm_points,
          inherit.aes = FALSE,
          color = "blue",
          size = .5,
          alpha = .6) +
  geom_sf(data = liquor$osm_points,
          inherit.aes = FALSE,
          color = "orange",
          size = .5,
          alpha = .6) +
  geom_sf(data = pub$osm_points,
          inherit.aes = FALSE,
          color = "purple",
          size = .5,
          alpha = .6) +
  geom_sf(data = breweries$osm_polygons,
          inherit.aes = FALSE,
          color = "yellow",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = toronto[1,], 
           ylim = toronto[2,],
           expand = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5), 
        legend.position = "right", legend.box = "vertical",
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "WHERE TO GET BEER IN TORONTO", subtitle = "43.58025° N/ 79.63927° W")

#montreal data
montreal <- getbb("Montreal Canada")

mtlroads <- montreal%>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary", "motorway_link", "trunk_link", "primary_link")) %>%
  osmdata_sf()

mtlroads2 <- montreal%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

mtlroads3 <- montreal%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

mtllake<- montreal%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "lake")) %>%
  osmdata_sf()

mtlcoast <- montreal%>%
  opq()%>%
  add_osm_feature(key = "natural", value="coastline") %>%
  osmdata_sf()

mtlwaters <- montreal%>%
  opq()%>%
  add_osm_feature(key = "natural", value="water") %>%
  osmdata_sf()

mtltrain <- montreal%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

#montreal base map
font_add_google(name = "Lato", family = "lato") 
showtext_auto()
ggplot() +
  geom_sf(data = mtlcoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtlcoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtlwaters$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtlwaters$osm_polygon,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtllake$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtllake$osm_polygon,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtltrain$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = mtlroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = mtlroads2$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = mtlroads3$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = montreal[1,], 
           ylim = montreal[2,],
           expand = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "MONTREAL", subtitle = "45.41008° N/ 73.97416° W")

#montreal bakeries
bakeries <- montreal%>%
  opq()%>%
  add_osm_feature(key = "shop", value=c("bakery")) %>%
  osmdata_sf()

bakeries

font_add_google(name = "Lato", family = "lato") 
showtext_auto()
ggplot() +
  geom_sf(data = mtlcoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtlcoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtlwaters$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtlwaters$osm_polygon,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtllake$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtllake$osm_polygon,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = mtltrain$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = mtlroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = mtlroads2$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = mtlroads3$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = bakeries$osm_points,
          inherit.aes = FALSE,
          color = "deeppink",
          size = 1,
          alpha = .6) +
  coord_sf(xlim = montreal[1,], 
           ylim = montreal[2,],
           expand = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "BAKERIES IN MONTREAL", subtitle = "45.41008° N/ 73.97416° W")

#vancouver data
vancouver <- getbb("Vancouver Canada")

vroads <- vancouver%>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary", "motorway_link", "trunk_link", "primary_link")) %>%
  osmdata_sf()

vroads2 <- vancouver%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

vroads3 <- vancouver%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

vlakes <- vancouver%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "lake")) %>%
  osmdata_sf()

vcoast <- vancouver%>%
  opq()%>%
  add_osm_feature(key = "natural", value="coastline") %>%
  osmdata_sf()

vwaters <- vancouver%>%
  opq()%>%
  add_osm_feature(key = "natural", value="water") %>%
  osmdata_sf()

vtrain <- vancouver%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

#vancouver base map
font_add_google(name = "Lato", family = "lato") 
showtext_auto()
ggplot() +
  geom_sf(data = vcoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vcoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vwaters$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vwaters$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vlakes$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vlakes$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vtrain$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = vroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = vroads2$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = vroads3$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = vancouver[1,], 
           ylim = vancouver[2,],
           expand = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "VANCOUVER", subtitle = "49.19845° N/ 123.22496° W")

#vancouver seafood
seafood <- vancouver%>%
  opq()%>%
  add_osm_feature(key = "shop", value=c("seafood")) %>%
  osmdata_sf()

seafood

font_add_google(name = "Lato", family = "lato") 
showtext_auto()
ggplot() +
  geom_sf(data = vcoast$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vcoast$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vwaters$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vwaters$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vlakes$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vlakes$osm_polygons,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 1) +
  geom_sf(data = vtrain$osm_lines,
          inherit.aes = FALSE,
          color = "red",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = vroads$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = vroads2$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = vroads3$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  geom_sf(data = seafood$osm_points,
          inherit.aes = FALSE,
          color = "limegreen",
          size = 2,
          alpha = .6) +
  coord_sf(xlim = vancouver[1,], 
           ylim = vancouver[2,],
           expand = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "SEAFOOD SHOPS IN VANCOUVER", subtitle = "49.19845° N/ 123.22496° W")
