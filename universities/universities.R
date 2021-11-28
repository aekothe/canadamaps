#Is Canada Younger than the Flagship Univeristy?
#Angela Kothe
#06.10.21

library(devtools)
library(maps)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)
library(usmap)

setwd("~/Desktop/r/universities")

#data
data <- read_csv(file="universities.csv")
states <- map_data("state")
data$region <- data$state

?map_data

z <- plot_usmap(regions = "states"); z

state <- subset(states, select=c(lat, long, group, order, region))
data$region <- tolower(data$region)

map <- left_join(state, data, by = "region")

map$year <- factor(map$year, levels = c("Yes", "1 Year Older", 
                                        "2 Years Older", "5 Years Older",
                                        "10 Years Older", "15 Years Older",
                                        "20 Years Older", "25 Years Older"))



z <- ggplot(data= na.omit(map), aes(x=long, y=lat, fill=year, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3); z

y <- z +       geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map(); y

#map, add HI and AK
a <- ggplot(data = na.omit(map),
            aes(x = long, y = lat,
                group = group, fill = year)) +
      geom_polygon(color = "gray90", size = 0.1) +
      ggtitle("Is Canada Younger than the Flagship Univeristy?") +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme_map(); a

b <- a + scale_fill_manual(name = "",
                           values = c("violetred4", "violetred", 
                                      "hotpink3", "pink", "purple", "violet","plum3", 
                                      "thistle2")); b


