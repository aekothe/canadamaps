#Dot Density Map
#Angela Kothe
#12.03.21

library(maptools)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)
library(mapcan)
library(sf)

setwd("/Users/annkothe/Documents/GitHub/canadamaps")

#national map
canada <- mapcan(boundaries = ridings,
                 type = standard)

#national data
data <- read_dta("elections1997to2019.dta")
data <- filter(data, year == 2019)
data <- subset(data, select=c(code, province, french, english, population))

canada$code <- canada$riding_code
canada <- subset(canada, select=c(code, long, lat, order, hole, piece, group))
candata <- inner_join(canada, data, by = "code")

num.dots <- select(candata, population)/10

sp.dfs <- lapply(names(num.dots), function(x) {
  dotsInPolys(candata, as.integer(num.dots[, x]), f = "random")
})


ggplot(num.dots, aes(long, lat, group = group)) +
          geom_polygon() +
          theme_mapcan() +
          coord_fixed()







