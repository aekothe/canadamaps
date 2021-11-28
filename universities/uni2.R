library(devtools)
library(maps)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)
library(usmap)
library(extrafont)

setwd("~/Desktop/r/universities")

#data
data <- read_csv(file="universities.csv")

data$year <- factor(data$year, levels = c("Yes", "1 Year Older", 
                                          "2 Years Older", "5 Years Older",
                                          "10 Years Older", "15 Years Older",
                                          "20 Years Older", "25 Years Older", 
                                          "50 Years Older"))

plot_usmap(data = data, values = "year", color = "white") + 
  scale_fill_manual(name = "", breaks = c("Yes", "1 Year Older", 
                                          "2 Years Older", "5 Years Older",
                                          "10 Years Older", "15 Years Older",
                                          "20 Years Older", "25 Years Older", 
                                          "50 Years Older"),
                    values = c("violetred4", "violetred", 
                               "hotpink3", "pink", "purple", "violet","plum3", 
                               "thistle2", "magenta")) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=12, family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 40),
        legend.text = element_text(color = "black", size = 40)) +
  labs(title = "Is Canada Younger than the Flagship Univeristy?",
       subtitle = "") +
  theme(legend.position = "right")
