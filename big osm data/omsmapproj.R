

library(remotes)
library(sf)
library(tidyverse)
library(osmdata)
library(showtext)
library(ggmap)

c <- getbb("Manitoba, Canada")

b <- getbb("Alberta, Canada")

bb <- getbb("Canada", featuretype = "country")

coastline <- bb%>%
  opq()%>%
  add_osm_feature(key = "natural", value="coastline") %>%
  osmdata_sf()

water <- c%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "lake")) %>%
  osmdata_sf()

roads <- bb%>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("primary")) %>%
  osmdata_sf()


font_add_google(name = "Lato", family = "lato") 
showtext_auto()
ggplot() +
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "brown",
          size = 1) +
  theme_void() + 
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", 
                                     size = 8, hjust=.5, 
                                     margin=margin(2, 0, 5, 0))) +
  labs(title = "", subtitle = "")

