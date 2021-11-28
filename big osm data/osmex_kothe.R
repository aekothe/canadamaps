#Canada OSM Big Data
#Angela Kothe

library(remotes)
library(sf)
library(tidyverse)
library(osmdata)
library(showtext)
library(ggmap)
library(osmextract)

setwd("~/Desktop/r/osmex")
options(timeout = 300)

#cycleways
cycleways <- oe_get("Canada", quiet = FALSE, 
                    query = "SELECT * FROM 'lines' WHERE highway = 'cycleway'")

par(mar = rep(0.1, 4))
plot(sf::st_geometry(cycleways))

#rivers
rivers <- oe_get("Canada", quiet = FALSE, 
                    query = "SELECT * FROM 'lines' WHERE waterway = 'river'")

par(mar = rep(0.1, 4))
plot(sf::st_geometry(rivers), col = "steelblue", main = "Rivers of Canada")


#lakes, not working, no such column error
lakes <- oe_get("Canada", quiet = FALSE, 
                 query = "SELECT * FROM 'lines' WHERE natural = 'water'")

par(mar = rep(0.1, 4))
plot(sf::st_geometry(lakes))

#beware, trouble running 9mil+ observations, supposed to draw Canada
c_lines <- oe_get("Canada", stringsAsFactors = FALSE, quiet = TRUE)
c_points <- oe_get("Canada", layer = "points", stringsAsFactors = FALSE, quiet = TRUE)
par(mar = rep(0, 4))
plot(st_geometry(c_lines), xlim = c(-1.59, -1.1), ylim = c(50.5, 50.8))
plot(st_geometry(c_points), xlim = c(-1.59, -1.1), ylim = c(50.5, 50.8))
