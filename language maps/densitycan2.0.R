#Dot Density Map 2.0
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
canada <- mapcan(boundaries = census,
                 type = standard)

#national data
data <- mapcan::census_pop2016

#national dataset with map
canada$census_code <- canada$census_division_code

shp <- merge(canada, data, by = "census_division_name")

candata <- inner_join(canada, data, by = "census_code")

#cleaner national dataset
candata$name <- candata$census_division_name.x
candata$code <- candata$census_code
candata$prcode <- candata$pr_sgc_code.x
candata$accronym <- candata$pr_alpha.x
candata$province <- candata$pr_english.x
candata$type <- candata$census_division_type.y
candata$pop2016 <- candata$population_2016

census <- subset(candata, select=c(long, lat, order, hole, piece, group, 
                                   name, code, type, prcode, accronym, 
                                   province, pop2016))

census$dots <-  census$pop2016/10

dots <- dotsInPolys(census, as.integer(census$dots), f = "random", 
                         offset, compatible = FALSE)

#
shp$plotvar <- as.integer(shp$population_2016 / 1000)
shp <- shp[!is.na(shp$plotvar), ]

sccdots.rand <- dotsInPolys(shp, shp$plotvar, f="random", offset, 
                            compatible = FALSE)


par(mar=c(0,0,0,0))
plot(canada, lwd=0.1)
plot(sccdots.rand, add=TRUE, pch=19, cex=0.1, col="#00880030")



