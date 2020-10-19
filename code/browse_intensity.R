# browse intensity

library(tidyverse)
library(here)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

herb <- read.csv(here("herb.csv"), stringsAsFactors = F)
herb$BROW_INDEX <- as.numeric(herb$BROW_INDEX)
b_index <- herb %>%
  drop_na(BROW_INDEX) %>%
  select(TREAT, SITE, SITECODE, BROW_INDEX)

test <- c("DALTON" = "Upland", "STEESE" = "Lowland")
b_intens_plot <- ggplot(b_index, aes(x = as.factor(TREAT), y = BROW_INDEX)) + 
  geom_boxplot() + facet_wrap(~SITE, labeller = as_labeller(test)) + 
  labs(title = "Browsing Intensity", x = "Number of Fires",
       y = "Index of Browse Intensity")
save_plot("b_intens.png", b_intens_plot, base_aspect_ratio = 1.5)  

  
