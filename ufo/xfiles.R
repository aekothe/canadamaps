#Angela Kothe
#UFO Sightings
#05.30.21

library(devtools)
library(mapcan)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)
library(gganimate)
library(ggmap)
library(dplyr)
library(animation)
library(glue)
library(sf)
library(rgdal)

setwd("~/Desktop/r/ufo")

#data
map <- readOGR("lpr_000b16a_e.shp")
data <- read_csv(file="nuforc_reports.csv")

#first convert the coordinates to long/lat
map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
map.df <- fortify(map)

#clean data
data$lat <- data$city_latitude
data$long <- data$city_longitude
data$year <- data$date_time
data$pr_alpha <- data$state

ufo <- subset(data, select=c(lat, long, year, pr_alpha, city))

#keep on first four characters of row
ufo$year <- substr(ufo$year, 0, 4)

#filter by country and data availability
ufo <- filter(ufo, pr_alpha == "ON" | pr_alpha == "AB" | pr_alpha == "NB" | pr_alpha == "QC" 
              | pr_alpha == "NL" | pr_alpha == "NS" | pr_alpha == "PE" | pr_alpha == "BC"
              | pr_alpha == "SK" | pr_alpha == "MB" | pr_alpha == "YT" | pr_alpha == "NT" 
              | pr_alpha == "NU")

ufo <- na.omit(ufo)

#map (a), points (b), gif (c)
a <- ggplot(map.df, aes(x = long, y = lat, group = group)) +
      geom_polygon(colour = alpha("white", .1), size = 0.3, fill = "black") +
      coord_map(projection = "albers",  lat0 = 49, lat1 = 75) +
      theme_map(); a

b <- a + geom_point(data = ufo, aes(x = long, y = lat), size = 1, 
                    color = "yellow",
                    inherit.aes = FALSE) +
          labs(caption = "source: National UFO Reporting Center"); b

c <- b + labs(title = 'UFO Sightings in {closest_state}') +
          transition_states(year,
                            transition_length = 2,
                            state_length = 1) + 
          ease_aes('cubic-in-out'); c

anim_save("xfile.gif", c)
anim_save("xfile.mp4", c)
