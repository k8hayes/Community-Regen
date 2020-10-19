# processing sprouteration traits

library(tidyverse)
library(ggplot2)
library(cowplot)
library(here)
theme_set(theme_cowplot())

# merging dbh
  stee <- read.csv(here("Steese_DBH.csv"))
  dalt <- read.csv(here("Dalton_DBH.csv"))
  dbh <- rbind(dalt, stee) ; rm(dalt, stee)
  write.csv(dbh, "dbh.csv", row.names = F)

dbh <- read.csv(here("dbh.csv"), stringsAsFactors = F)

dbh <- dbh[dbh$SPP != "POBA",]
dbh <- dbh[dbh$SPP != "PIGL",]
dbh <- dbh[dbh$SPP != "ARCTO",]
dbh <- dbh[dbh$SPP != "UNKNOWN",]

sprout <- dbh %>%
  group_by(TREAT, SITE,SITECODE, SPP, STEM_TYPE) %>%
  summarise(COUNT = n()) %>%
  ungroup() %>%
  complete(TREAT, SITE,SITECODE, SPP, STEM_TYPE, fill = list(COUNT = 0))

total <- dbh %>%
  group_by(TREAT, SITE,SITECODE, SPP) %>%
  summarise(COUNT = n()) %>%
  ungroup() %>%
  complete(TREAT, SITE, SITECODE, SPP, fill = list(COUNT = 0)) %>%
  slice(rep(1:n(), each = 2))

sprout$total <- NA
sprout$total <- total$COUNT
sprout$index <- NA
sprout$index <- sprout$COUNT / sprout$total

sprout$index[is.nan(sprout$index)] <- 0
sprout$index <- round(sprout$index, digits = 2)

sprout %>%
  filter(STEM_TYPE == "CLUMP") %>%
  ggplot(aes(x = as.factor(TREAT), y = index, col = SPP)) + 
  geom_boxplot()    

regen_strat_plot <- sprout %>%
  ggplot(aes(x = as.factor(TREAT), y = index, fill = STEM_TYPE)) + 
  geom_histogram(position = "fill", stat = "identity") + 
  labs(x = "Number of Fires", y = "Relative Presence",
       title = "Tree Regeneration Strategies") + 
  scale_fill_manual(name = "Type of Reproduction",
                    labels = c("Asexual", "Sexual"),
                    values = c("#bdbdbd", "#f0f0f0"))
save_plot("regen_plot.png", regen_strat_plot, base_aspect_ratio = 1.5)

## ADD STEESE CONTROLS