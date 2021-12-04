#Canadian Language Data
#Angela Kothe
#12.02.2021

library(devtools)
library(mapcan)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)

library(sf)
library(rgdal)
library(transformr)
library(ggmap)
library(dplyr)
library(viridis)
library(haven)

setwd("/Users/annkothe/Documents/GitHub/canadamaps/language maps")

#national map
canada <- mapcan(boundaries = ridings,
                 type = standard)

#national data
data <- read_dta("elections1997to2019.dta")
data <- filter(data, year == 2019)
data <- subset(data, select=c(code, province, french, population))

#national dataset with map
canada$code <- canada$riding_code
canada <- subset(canada, select=c(code, long, lat, order, hole, piece, group))
candata <- inner_join(canada, data, by = "code")
candata$pctFrench <- candata$french/candata$population

#country
natmap <- ggplot(candata, aes(long, lat, group = group, fill = pctFrench)) +
            scale_fill_continuous(low = "white", high = "blue", 
                                  name = "percentage", label = scales::comma) +
            geom_polygon(color = "black", size = .1) +
            theme_mapcan() +
            coord_fixed() +
            theme(legend.position = "none") +
            ggtitle(""); natmap

nm <- ggplot(candata, aes(long, lat, group = group, fill = pctFrench)) +
        geom_polygon(color = "black", size = .1) +
        scale_fill_viridis(option = "magma", direction = -1, " ") +
        theme_mapcan() +
        coord_fixed() +
        theme(legend.position = "right") +
        ggtitle("Francophones as a Population Proportion"); nm

#filter for three
three <- filter(candata, province == "ON" | province == "NB" | province == "QC")

m3 <- ggplot(three, aes(long, lat, group = group, fill = pctFrench)) +
        geom_polygon(color = "black", size = .1) +
        scale_fill_continuous(low = "white", high = "red", 
                              name = "", label = scales::comma) +
        theme_mapcan() +
        coord_fixed() +
        theme(legend.position = "right") +
        ggtitle("Francophones as a Population Proportion"); m3

#filter for four
four <- filter(candata, province == "ON" | province == "NB" | province == "QC"
                | province == "MB")

m4 <- ggplot(four, aes(long, lat, group = group, fill = pctFrench)) +
        geom_polygon(color = "black", size = .1) +
        scale_fill_continuous(low = "white", high = "red", 
                              name = "", label = scales::comma) +
        theme_mapcan() +
        coord_fixed() +
        theme(legend.position = "right") +
        ggtitle("Francophones as a Population Proportion"); m4

m <- ggplot(four, aes(long, lat, group = group, fill = pctFrench)) +
      geom_polygon(color = "black", size = .1) +
      scale_fill_viridis(option = "magma", direction = -1) +
      theme_mapcan() +
      coord_fixed() +
      theme(legend.position = "right") +
      ggtitle("Francophones as a Population Proportion"); m

