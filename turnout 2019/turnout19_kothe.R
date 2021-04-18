#Turnout 2019
#Angela Kothe

library(devtools)
library(mapcan)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)

#national map
ridings <- mapcan(boundaries = ridings,
                  type = standard)

riding_results <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/2019simplified.csv")
riding_results$riding_code <- riding_results$ridingNumber
map2019 <- inner_join(ridings, riding_results, by = "riding_code")

ggplot(map2019, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout by Riding, 2019")

#Alberta
abridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = AB)
abmap <- left_join(abridings, riding_results, "riding_code")

ggplot(abmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Alberta, 2019")

#Calgary
calridings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/calgary.csv")
calmap <- left_join(calridings, map2019, by = "riding_name_english")

ggplot(calmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Turnout in Calgary, 2019")

#Edmonton
edridings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/edmonton.csv")
edmap <- left_join(edridings, map2019, by = "riding_name_english")

ggplot(edmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Edmonton, 2019")

#British Columbia
bcridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = BC)
bcmap <- left_join(bcridings, riding_results, "riding_code")

ggplot(bcmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in British Columbia, 2019")

#Greater Vancouver
vancouverridings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/vancouver.csv")
vanmap <- left_join(vancouverridings, map2019, by = "riding_name_english")

ggplot(vanmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Vancouver, 2019")

#Manitoba
mbridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = MB)
mbmap <- left_join(mbridings, riding_results, "riding_code")

ggplot(mbmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Manitoba, 2019")

#Winnipeg
winnipeg <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/winnipeg.csv")
wmap <- left_join(winnipeg, map2019, by = "riding_name_english")

ggplot(wmap, aes(long, lat, group = group, fill = pctVote)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Winnipeg, 2019")

#New Brunswick
nbridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = NB)
nbmap <- left_join(nbridings, riding_results, "riding_code")

ggplot(nbmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in New Brunswick, 2019") +
  theme(legend.position = "right")

#New Foundland and Labrador
nlridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = NL)
nlmap <- left_join(nlridings, riding_results, "riding_code")

ggplot(nlmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in New Foundland and Labrador, \n2019")  +
  theme(legend.position = "right")

#Northwest Territories
nwtridings <- mapcan(boundaries = ridings,
                     type = standard,
                     province = NT)
nwtmap <- left_join(nwtridings, riding_results, "riding_code")

ggplot(nwtmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout Northwest Territories, 2019")

#Nova Scotia
nsridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = NS)
nsmap <- left_join(nsridings, riding_results, "riding_code")

ggplot(nsmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Nova Scotia, 2019")

#Nunavut
nuridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = NU)
numap <- left_join(nuridings, riding_results, "riding_code")

ggplot(numap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Nunavut, 2019")

#Ontario
onridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = ON)
onmap <- left_join(onridings, riding_results, "riding_code")

ggplot(onmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Ontario, 2019")

#Greater Toronto
torontoridings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/gtaridings.csv")
gta <- left_join(torontoridings, map2019, by = "riding_name_english")

ggplot(gta, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Toronto, 2019")

#London
londonridings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/london.csv")
london <- left_join(londonridings, map2019, by = "riding_name_english")

ggplot(london, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in London, 2019") +
  theme(legend.position = "right")

#Kitchener
kitchenerridings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/kitchener.csv")
kitchener <- left_join(kitchenerridings, map2019, by = "riding_name_english")

ggplot(kitchener, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Kitchener, 2019")

#Windsor
windsorridings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/windsor.csv")
windsor <- left_join(windsorridings, map2019, by = "riding_name_english")

ggplot(windsor, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Windsor, 2019")

#Hamilton Niagara
niagararidings <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/hamiltonniagara.csv")
niagara <- left_join(kitchenerridings, map2019, by = "riding_name_english")

ggplot(niagara, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Hamilton-Niagara, 2019")

#Prince Edward Island
peiridings <- mapcan(boundaries = ridings,
                     type = standard,
                     province = PE)
peimap <- left_join(peiridings, riding_results, "riding_code")

ggplot(peimap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Prince Edward Island, 2019") +
  theme(legend.position = "right")

#Quebec
qcridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = QC)
qcmap <- left_join(qcridings, riding_results, "riding_code")

ggplot(qcmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Quebec, 2019")

#Greater Montreal
montreal <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/greatermontreal.csv")

gma <- left_join(montreal, map2019, by = "riding_name_english")

ggplot(gma, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Montreal, 2019")

#Quebec City
quebec <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/quebeccity.csv")

quebecmap <- left_join(quebec, map2019, by = "riding_name_english")

ggplot(quebecmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Quebec City, 2019")

#Saskatchewan
skridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = SK)
skmap <- left_join(skridings, riding_results, "riding_code")

ggplot(skmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Saskatchewan, 2019")

#Regina
regina <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/regina.csv")

reginamap <- left_join(regina, map2019, by = "riding_name_english")

ggplot(reginamap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Regina, 2019")

#Saskatoon
saskatoon <- read_csv(file="/Users/annkothe/desktop/r/mapcan/data/saskatoon.csv")

saskmap <- left_join(saskatoon, map2019, by = "riding_name_english")

ggplot(saskmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Saskatoon, 2019")

#Yukon
ytridings <- mapcan(boundaries = ridings,
                    type = standard,
                    province = YT)
ytmap <- left_join(ytridings, riding_results, "riding_code")

ggplot(ytmap, aes(long, lat, group = group, fill = voterTurnout)) +
  geom_polygon() +
  scale_fill_viridis_c(name = "") +
  theme_mapcan() +
  coord_fixed() +
  ggtitle("Voter Turnout in Yukon, 2019")