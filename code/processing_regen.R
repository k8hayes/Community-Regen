# processing regeneration traits

library(tidyverse)
library(ggplot2)
library(cowplot)
library(here)
theme_set(theme_cowplot())


dbh <- read.csv(here("data/dbh.csv"), stringsAsFactors = F)

dbh <- dbh[dbh$SPP != "POBA",]
dbh <- dbh[dbh$SPP != "PIGL",]
dbh <- dbh[dbh$SPP != "ARCTO",]
dbh <- dbh[dbh$SPP != "UNKNOWN",]

sprout <- dbh %>%
  rename("STEM_TYPE" = "CLUMP")

sprout <- sprout %>%
  group_by(TREAT, SITE, PLOT, SPP, STEM_TYPE) %>%
  summarise(COUNT = n()) %>%
  ungroup() %>%
  complete(TREAT, SITE,PLOT, SPP, STEM_TYPE, fill = list(COUNT = 0))

total <- dbh %>%
  group_by(TREAT, SITE,PLOT, SPP) %>%
  summarise(COUNT = n()) %>%
  ungroup() %>%
  complete(TREAT, SITE, PLOT, SPP, fill = list(COUNT = 0)) %>%
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

 sprout %>%
  ggplot(aes(x = as.factor(TREAT), y = index, fill = STEM_TYPE)) +
  facet_wrap(~SITE) +
  geom_histogram(position = "fill", stat = "identity") +
  labs(x = "Number of Fires", y = "Relative Presence",
       title = "Tree Regeneration Strategies") +
  scale_fill_manual(name = "Type of Reproduction",
                    labels = c("Asexual", "Sexual"),
                    values = c("#bdbdbd", "#f0f0f0"))

av_index <- sprout %>%
  group_by(SITE, TREAT, PLOT, STEM_TYPE) %>%
  summarise(AV = mean(index), SD = sd(index))

count %>%
  ggplot(aes(x = as.factor(TREAT), y = index, fill = STEM_TYPE)) +
  facet_wrap(~SITE) +
  geom_histogram(position = "fill", stat = "identity") +
  labs(x = "Number of Fires", y = "Relative Presence",
       title = "Tree Regeneration Strategies") +
  scale_fill_manual(name = "Type of Reproduction",
                    labels = c("Asexual", "Sexual"),
                    values = c("#bdbdbd", "#f0f0f0"))

dalt <- sprout %>%
  filter(SITE == "DALTON") %>%
  ggplot(aes(x = as.factor(TREAT), y = index, color = STEM_TYPE)) +
  geom_point()  + geom_smooth( aes(group = STEM_TYPE), method = lm, se = F) +
  labs(x = "Number of Fires", y = "Relative Presence",
       title = "Upland Trait Regeneration") +
  scale_color_manual(name = "Regeneration Trait",
                    labels = c("Asexual", "Sexual"),
                    values = c("#fd8d3c", "#41b6c4")) +
  theme(legend.position = "none")
stee <- sprout %>%
  filter(SITE == "STEESE") %>%
  ggplot(aes(x = as.factor(TREAT), y = index, color = STEM_TYPE)) +
  geom_point()  + geom_smooth( aes(group = STEM_TYPE), method = lm, se = F) +
  labs(x = "Number of Fires", y = "Relative Presence",
       title = "Lowland Trait Regeneration") +
  scale_color_manual(name = "Regeneration Trait",
                     labels = c("Asexual", "Sexual"),
                     values = c("#fd8d3c", "#41b6c4"))
plot_grid(dalt, stee, rel_widths = c(1,1.5))

save_plot("regen_plot.png", regen_strat_plot, base_aspect_ratio = 1.5)

av_count <- sprout %>%
  group_by(SITE, TREAT, PLOT, STEM_TYPE) %>%
  summarise(AV = mean(COUNT))

av_index1 <-  sprout %>%
  group_by(SITE, TREAT, STEM_TYPE) %>%
  summarise(AV = mean(index), SD = sd(index))

av_index1 %>%
  ggplot(aes(x = as.factor(TREAT), y = AV, col = STEM_TYPE)) +
  geom_line(aes(group = STEM_TYPE)) + facet_wrap(~SITE) +
  geom_errorbar(aes(ymin = SD, ymax = SD)) +
  labs(x = "Number of Fires", y = "Relative Presence",
       title = "Tree Regeneration Strategies") +
  scale_color_manual(name = "Type of Reproduction",
                     labels = c("Asexual", "Sexual"),
                     values = c("#fd8d3c", "#41b6c4"))


## ADD STEESE CONTROLS
