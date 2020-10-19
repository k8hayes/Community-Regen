# proportion of browsing plot
library(tidyverse)
library(here)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

herb <- read.csv(here("herb.csv"))

tot_brow <- herb %>%
  group_by(SITE, TREAT,SITECODE, B_TYPE) %>%
  summarize(N = n()) %>%
  ungroup() %>%
  complete(SITE, TREAT, SITECODE, B_TYPE, fill = list(N=0))
stee_plot <- tot_brow %>%
  filter(SITE != "DALTON") %>%
  ggplot(aes(x = as.factor(TREAT), y = N, fill = B_TYPE)) + 
  geom_histogram(position="fill", stat="identity") + 
  scale_fill_manual(name = "Browse Type",
                    labels = c("Both Moose/Hare", "Hare", "Moose", "Neither"),
                    values = c("cadetblue", "chocolate", "brown", "coral")) + 
  labs(title = "Lowland Site", x = "Number of Fires",
       y = " ")
dalt_plot <- tot_brow %>%
  filter(SITE != "STEESE") %>%
  ggplot(aes(x = as.factor(TREAT), y = N, fill = B_TYPE)) + 
  geom_histogram(position="fill", stat="identity") + 
  scale_fill_manual(name = "Browse Type",
                    labels = c("Both Moose/Hare", "Hare", "Moose", "Neither"),
                    values = c("cadetblue", "chocolate", "brown", "coral")) + 
  labs(title = "Upland Site", x = "Number of Fires",
       y = "Proportion of Browse Type") + 
  theme(legend.position = "none")
herb_plot <- plot_grid(dalt_plot, stee_plot, nrow = 1, 
                       rel_widths = c(1,1.75), labels = c("B."))
herb_plot
# save_plot("herb_plot.png",herb_plot) # save as png # 650 by 300

# checking how many sites are browsed
test <- herb %>%
  group_by(SITE, TREAT,SITECODE, B_TYPE) %>%
  summarize(N = n()) 