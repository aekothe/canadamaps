#Canadian Language Data
#Angela Kothe
#04.30.2021

library(devtools)
library(mapcan)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)

#national map
map <- mapcan(boundaries = census,
                    type = standard)

ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_fixed() +
  theme_mapcan() +
  ggtitle("Federal Census District")

data <- mapcan::census_pop2016

map$census_code <- map$census_division_code
canada <- inner_join(map, data, by = "census_code")

ggplot(canada, aes(long, lat, group = group, fill = population_2016)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "Population") +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("Population")

ggplot(canada, aes(long, lat, group = group, fill = land_area_2016)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "Land Area Map") +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("Land Area Map")


#Québec
qc <- mapcan(boundaries = census,
                   type = standard,
              province = QC)

qc$census_code <- qc$census_division_code
qcdata <- inner_join(qc, data, by="census_code")
qcdata$code <- qcdata$census_division_code
qcdata$name <- qcdata$census_division_name.x

ggplot(qcdata, aes(long, lat, group = group, fill = population_2016)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "Population") +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("Population Map")

lang <- read_csv(file="/Users/annkothe/desktop/r/mapcan/language/languagedata.csv")

langdata <- subset(lang, select=c(riding_code, party))