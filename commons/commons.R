#Canada Commons
#Angela Kothe

library(devtools)
library(mapcan)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)
library(extrafont)
library(grid)
library(gridExtra)
library(RColorBrewer)

setwd("~/Desktop/r/mapcan/commons")

#data
provinces <- mapcan(boundaries = provinces,
                          type = standard)

commons <- read_csv(file="commons.csv")

data <- left_join(provinces, commons, by = "pr_alpha")

data$approxPersons <- factor(data$approxPersons, levels = c("130 Thousand", "120 Thousand", 
                                                            "110 Thousand", "100 Thousand",
                                                            "90 Thousand", "80 Thousand",
                                                            "70 Thousand", "40 Thousand"))

#map
map <- ggplot(data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = approxPersons), colour = alpha("white", 1), size = .3) +
  coord_fixed() +
  theme_minimal() +
  ggtitle("APPROXIMATE NUMBER OF PERSONS PER RIDING") +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=12, family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 40),
        legend.text = element_text(color = "black", size = 40)) +
  scale_fill_manual(name = "Persons per Riding",
                    values = c("red4", "red3", "orangered", "tomato3",
                               "palevioletred3", "salmon2", "orange", 
                               "gold")); map

#Prince Edward Island
pei <- data%>%
  filter(pr_sgc_code == 11)

pmap <- pei%>%
  ggplot(aes(x = long, y = lat, group = group, fill = approxPersons)) +
  geom_polygon(colour = alpha("white", 1), size = 0.3) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=8, family = "serif"),
        legend.position = "none",
        panel.border = element_rect(fill = NA)) +
  ggtitle("") +
  scale_fill_manual(name = "Persons per Riding",
                    values = c("gold")); pmap

#final
map
print(pmap, vp = viewport(.85, .08, width = 0.25, height = 0.25))