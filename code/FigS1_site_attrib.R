# Making histograms of site attributes
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

site <- read.csv(here("data/site_attrib.csv"))

site <- site %>%
  filter(SITE == "DALTON")

site$TREAT <- as.factor(site$TREAT)

site %>%
  group_by(TREAT) %>%
  summarise(slopeAV = mean(SLOPE),
            slopeSD = sd(SLOPE, na.rm = T),
            elevAV = mean(ELEVATION),
            elevSD = sd(ELEVATION),
            solarAV = mean(SOLAR),
            solarSD = sd(SOLAR))
