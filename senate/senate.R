#Canada Senate
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

#data
provinces <- mapcan(boundaries = provinces,
                          type = standard)

senate <- read_csv(file="/Users/annkothe/desktop/r/data/senate.csv")

data <- left_join(provinces, senate, by = "pr_alpha")

#reordering legend
data$sPersons <- factor(data$sPersons, levels = c("800 Thousand", "700 Thousand", 
                                                  "600 Thousand", "300 Thousand",
                                                  "200 Thousand", "100 Thousand",
                                                  "80 Thousand", "40 Thousand"))

#national
map <- ggplot(data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = sPersons), colour = alpha("white", 1), size = .3) +
  coord_fixed() +
  theme_minimal() +
  ggtitle("APPROXIMATE NUMBER OF PERSONS PER SENATOR") +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=12, family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 40),
        legend.text = element_text(color = "black", size = 40)) +
  scale_fill_manual(name = "",
                    values = c("violetred4", "violetred", 
                               "hotpink3", "pink", "purple", "violet","plum3", 
                               "thistle2")); map

#Prince Edward Island
pei <- data%>%
       filter(pr_sgc_code == 11)

pmap <- pei%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = sPersons), colour = alpha("white", 1), size = 0.3)+
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
  scale_fill_manual(name = "Persons per Senator",
                    values = c("thistle2")); pmap

#final
map
print(pmap, vp = viewport(.85, .08, width = 0.25, height = 0.25))