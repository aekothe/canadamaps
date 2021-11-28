#Vote Percentage 2019, Pt. II
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

#national map
ridings <- mapcan(boundaries = ridings,
                  type = standard)

results <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/2019simplified.csv")
results$riding_code <- results$ridingNumber
map2019 <- inner_join(ridings, results, by = "riding_code")

#binary map
map2019$binary = as.integer(map2019$pctVote >= 50)
map2019$r2019[which(map2019$pctVote < 50)] = "No"
map2019$r2019[which(map2019$pctVote > 50)] = "Yes"

natblue <- map2019%>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
               axis.ticks = element_blank(), axis.title = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               text = element_text(size=12, family = "serif")) +
  ggtitle("Did the Winning Party Receive at Least 50% of the Vote?") +
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); natblue

#montreal
montreal <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/greatermontreal.csv")
mtl <- left_join(montreal, map2019, by = "riding_name_english")

mtlblue <- mtl%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3)+
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
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); mtlblue

natblue
print(mtlblue, vp = viewport(.89, .12, width = 0.25, height = 0.25))

#greater toronto
torontoridings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/gtaridings.csv")
gta <- left_join(torontoridings, map2019, by = "riding_name_english")

gtablue <- gta%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3)+
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
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); gtablue

natblue
print(mtlblue, vp = viewport(.89, .12, width = 0.25, height = 0.25))
print(gtablue, vp = grid::viewport(0.70, 0.12, width = 0.25, height = 0.25))

#hamilton
hamilton <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/hamiltonniagara.csv")
ham <- left_join(hamilton, map2019, by = "riding_name_english")

hamblue <- ham%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3)+
  coord_fixed() +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=8, family = "serif"),
        legend.position = "none",
        panel.border = element_rect(fill = NA)) +
  ggtitle("Hamilton") +
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); hamblue

#london
london <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/london.csv")
lon <- left_join(london, map2019, by = "riding_name_english")

lonblue <- lon%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3)+
  coord_fixed() +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=8, family = "serif"),
        legend.position = "none",
        panel.border = element_rect(fill = NA)) +
  ggtitle("London") +
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); lonblue

natblue
print(mtlblue, vp = viewport(.89, .12, width = 0.25, height = 0.25))
print(gtablue, vp = grid::viewport(.70, .12, width = 0.25, height = 0.25))
print(hamblue, vp = grid::viewport(.50, .0774, width = 0.25, height = 0.25))

#SK
saskatoon <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/saskatoon.csv")
sk <- left_join(saskatoon, map2019, by = "riding_name_english")

skblue <- sk%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3)+
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
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); skblue

natblue
print(mtlblue, vp = viewport(.89, .12, width = 0.25, height = 0.25))
print(gtablue, vp = grid::viewport(.70, .12, width = 0.25, height = 0.25))
print(hamblue, vp = grid::viewport(.50, .0774, width = 0.25, height = 0.25))
print(skblue, vp = grid::viewport(.280, .11, width = 0.25, height = 0.25))

regina <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/regina.csv")
rg <- left_join(regina, map2019, by = "riding_name_english")

rgblue <- rg%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3)+
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
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); rgblue

natblue
print(mtlblue, vp = viewport(.89, .12, width = 0.25, height = 0.25))
print(gtablue, vp = grid::viewport(.70, .12, width = 0.25, height = 0.25))
print(hamblue, vp = grid::viewport(.50, .0774, width = 0.25, height = 0.25))
print(skblue, vp = grid::viewport(.280, .11, width = 0.25, height = 0.25))
print(rgblue, vp = grid::viewport(.09, .11, width = 0.25, height = 0.25))

#AB
calgary <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/calgary.csv")
cal <- left_join(calgary, map2019, by = "riding_name_english")

calblue <- cal%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3)+
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
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); calblue

edmonton <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/edmonton.csv")
ed <- left_join(edmonton, map2019, by = "riding_name_english")

edblue <- ed%>%
  ggplot(aes(x = long, y = lat, group = group, fill = r2019)) +
  geom_polygon(aes(fill = r2019), colour = alpha("black", 1), size = 0.3)+
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
  scale_fill_manual(name = "",
                    values = c("grey", "lightblue")); edblue


natblue
print(mtlblue, vp = viewport(.89, .12, width = 0.25, height = 0.25))
print(gtablue, vp = grid::viewport(.70, .12, width = 0.25, height = 0.25))
print(hamblue, vp = grid::viewport(.50, .0774, width = 0.25, height = 0.25))
print(skblue, vp = grid::viewport(.280, .11, width = 0.25, height = 0.25))
print(rgblue, vp = grid::viewport(.09, .11, width = 0.25, height = 0.25))
print(calblue, vp = grid::viewport(.280, .11, width = 0.25, height = 0.25))
print(edblue, vp = grid::viewport(.09, .11, width = 0.25, height = 0.25))
print(lonblue, vp = grid::viewport(.09, .11, width = 0.25, height = 0.25))

#Clear, Option C Map
mapc <- ggplot(map2019, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = pctVote), colour = alpha("white", 1), size = .3) +
  coord_fixed() +
  theme_minimal() +
  ggtitle("Percentage of the Vote Received by Winning Party, 2019") +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=12, family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 40),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "bottom") +
  scale_fill_viridis_c(name = "Percentage", option = "C"); mapc
