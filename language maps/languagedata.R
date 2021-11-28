#Canadian Language Data
#Angela Kothe
#04.30.2021

library(devtools)
library(mapcan)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)

#making the trio map, other RAM issues

#national map
canada <- mapcan(boundaries = census,
                       type = standard)

#national data
data <- mapcan::census_pop2016

#national dataset with map
canada$census_code <- canada$census_division_code
candata <- inner_join(canada, data, by = "census_code")

#cleaner national dataset
candata$name <- candata$census_division_name.x
candata$code <- candata$census_code
candata$prcode <- candata$pr_sgc_code.x
candata$accronym <- candata$pr_alpha.x
candata$province <- candata$pr_english.x
candata$type <- candata$census_division_type.y
candata$pop2016 <- candata$population_2016

census <- subset(candata, select=c(long, lat, order, hole, piece, group, name, code, type,
                                     prcode, accronym, province, pop2016))

#language data
lang <- read_csv(file="/Users/annkothe/desktop/r/mapcan/language/languagedata.csv")

#clean it
lang$english2016 <- lang$`Dim: Language spoken most often at home (10): Member ID: [2]: English`
lang$french2016 <- lang$`Dim: Language spoken most often at home (10): Member ID: [3]: French`
lang$bilingual2016 <- lang$`Dim: Language spoken most often at home (10): Member ID: [7]: English and French`
lang$aboriginal2016 <- lang$`Dim: Language spoken most often at home (10): Member ID: [5]: Aboriginal`
lang$other2016 <- lang$`Dim: Language spoken most often at home (10): Member ID: [4]: Non-official language`
lang$name <- lang$GEO_NAME

langdata <- subset(lang, select=c(name, english2016, french2016, bilingual2016, aboriginal2016, 
                                  other2016))

language <- inner_join(census, langdata, by = "name")

#as a percentage of population dummy
language$pctFrench <- language$french2016/language$pop2016
language$pctEnglish <- language$english2016/language$pop2016

#per capita
language$capitaFrench <- language$pop2016/language$french2016
language$capitaEnglish <- language$pop2016/language$english2016

#fix quebec holes
qc <- read_csv(file="/Users/annkothe/desktop/r/mapcan/language/quebeclanguage.csv")
  
qlanguage <- inner_join(census, qc, by = "code")

#most spoken language, Quebec
qlanguage$mostSpoken[which(qlanguage$english > qlanguage$french)] = "English"
qlanguage$mostSpoken[which(qlanguage$english < qlanguage$french)] = "French"
qlanguage$mostSpoken[which(qlanguage$french < qlanguage$aboriginal)] = "Aboriginal"

quebec <- qlanguage%>%
  ggplot(aes(x = long, y = lat, group = group, fill = mostSpoken)) +
  geom_polygon(aes(fill = mostSpoken), colour = alpha("black", 1), size = 0.3)+
  coord_fixed() +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=8, family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 40)) +
  ggtitle("Most Spoken Language by Census Division") +
  scale_fill_manual(name = "",
                    values = c("red", "white", "blue")); quebec

#most spoken, Ontario
mson <- language %>%
  filter(province == "Ontario")

mson$mostSpoken <- NA

mson$mostSpoken[which(mson$english2016 > mson$french2016)] = "English"
mson$mostSpoken[which(mson$english2016 < mson$french2016)] = "French"
mson$mostSpoken[which(mson$english2016 < mson$aboriginal2016)] = "Aboriginal"

mSpokenOn <- mson%>%
  ggplot(aes(x = long, y = lat, group = group, fill = mostSpoken)) +
  geom_polygon(aes(fill = mostSpoken), colour = alpha("black", 1), size = 0.3)+
  coord_fixed() +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(size=8, family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 40)) +
  scale_fill_manual(name = "",
                    values = c("red", "white", "blue")); mSpokenOn

#most spoken, NB
msnb <- language %>%
  filter(province == "New Brunswick")

msnb$mostSpoken <- NA

msnb$mostSpoken[which(msnb$english2016 > msnb$french2016)] = "English"
msnb$mostSpoken[which(msnb$english2016 < msnb$french2016)] = "French"
msnb$mostSpoken[which(msnb$english2016 < msnb$aboriginal2016)] = "Aboriginal"

mSpokenNB <- msnb%>%
  ggplot(aes(x = long, y = lat, group = group, fill = mostSpoken)) +
  geom_polygon(aes(fill = mostSpoken), colour = alpha("black", 1), size = 0.3)+
  coord_fixed() +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(size=8, family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 40)) +
  scale_fill_manual(name = "",
                    values = c("red", "white", "blue")); mSpokenNB

#trio
trio <- read_csv(file="/Users/annkothe/desktop/r/mapcan/language/trio.csv")

x <- filter(language, accronym == "ON" | accronym == "NB" | accronym == "QC")

#fix quebec holes
qc <- read_csv(file="/Users/annkothe/desktop/r/mapcan/language/quebeclanguage.csv")

y <- full_join(x, qc, by = "code")

ggplot(x, aes(long, lat, group = group, fill = pctFrench)) +
  geom_polygon() +
  scale_fill_continuous(low = "white", high = "red", 
                        name = "percentage", label = scales::comma) +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("French Speakers Map")

ggplot(y, aes(long, lat, group = group, fill = pctFrench.x)) +
  geom_polygon() +
  scale_fill_continuous(low = "red", high = "blue", name = "percentage", 
                        label = scales::comma) +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("French Speakers Map")



#Québec
qc <- language %>%
  filter(province == "Quebec")

ggplot(qc, aes(long, lat, group = group, fill = pctFrench)) +
  geom_polygon() +
  scale_fill_continuous(low = "white", high = "red", name = "percentage", label = scales::comma) +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("French Speakers Map")

ggplot(qc, aes(long, lat, group = group, fill = pctFrench)) +
  geom_polygon() +
  scale_fill_continuous(low = "red", high = "blue", name = "percentage", label = scales::comma) +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("French Speakers Map")


#New Brunswick
nb <- language %>%
  filter(province == "New Brunswick")

ggplot(nb, aes(long, lat, group = group, fill = pctFrench)) +
  geom_polygon() +
  scale_fill_continuous(low = "white", high = "red", name = "percentage", label = scales::comma) +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("French Speakers Map")

ggplot(nb, aes(long, lat, group = group, fill = pctFrench)) +
  geom_polygon() +
  scale_fill_continuous(low = "red", high = "blue", name = "percentage", label = scales::comma) +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("French Speakers Map")

#Ontario
ontario <- language %>%
  filter(province == "Ontario")

ggplot(ontario, aes(long, lat, group = group, fill = pctFrench)) +
  geom_polygon() +
  scale_fill_continuous(low = "white", high = "red", name = "percentage", label = scales::comma) +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("French Speakers Map, Ontario")

ggplot(ontario, aes(long, lat, group = group, fill = pctFrench)) +
  geom_polygon() +
  scale_fill_continuous(low = "red", high = "blue", name = "percentage", label = scales::comma) +
  theme_mapcan() +
  coord_fixed() +
  theme(legend.position = "right") +
  ggtitle("French Speakers Map")
