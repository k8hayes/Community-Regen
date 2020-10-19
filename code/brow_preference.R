# graph of browsing preference
library(tidyverse)
library(here)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

herb <- read.csv(here("herb.csv"), stringsAsFactors = F)

# getting rid of poba, pigl, arctic birch
herb <- herb[herb$SPP != "POBA",]
herb <- herb[herb$SPP != "PIGL",]
herb <- herb[herb$SPP != "ARCTO",]

# duplicating "both" measurements to be in hare and moose
both <- herb[herb$B_TYPE == "B",]
no_both <- herb[herb$B_TYPE != "B",]
hare <- both; hare$B_TYPE[hare$B_TYPE =="B"] <- "H"
moose <- both; moose$B_TYPE[moose$B_TYPE == "B"] <- "M"
herb <- rbind(no_both, hare, moose)

## counting stems browsed
brow <- herb %>%
  filter(B_TYPE != "N") %>%
  group_by(TREAT, SITE, SPP, B_TYPE) %>%
  summarize(brow = n()) %>%
  ungroup() %>%
  complete(TREAT,SITE, SPP, B_TYPE, fill = list(brow=0)) 

# total stem count
total <- herb %>%
  group_by(TREAT, SITE, SPP) %>%
  summarize(COUNT = n()) %>%
  ungroup() %>%
  complete(TREAT, SITE, SPP, fill = list(COUNT=0)) %>%
  slice(rep(1:n(), each = 2))

herb_pref <- brow
herb_pref$total <- total$COUNT
herb_pref$pref <- NA
herb_pref$pref <- herb_pref$brow / herb_pref$total

herb_pref$pref[is.nan(herb_pref$pref)] <- 0
herb_pref$pref <- round(herb_pref$pref, digits = 2)
herb_pref <- subset(herb_pref, select = -c(brow, total)); rm(brow,total)
herb_pref$B_TYPE[herb_pref$B_TYPE == "H"] <- "Hare"
herb_pref$B_TYPE[herb_pref$B_TYPE == "M"] <- "Moose"
# write.csv(herb_pref, "herb_pref.csv", row.names = F)

# herb_pref <- read.csv(here("herb_pref.csv"))


ggplot(herb_pref, aes(x = SPP, y = pref, col = B_TYPE)) + 
  geom_boxplot()  + 
  labs(x = "Species", y = "Ratio of Browsed to Available")

herb_pref$B_TYPE <- with(herb_pref, reorder(B_TYPE, pref))

b_pref <- ggplot(herb_pref, aes(x = B_TYPE, y = pref, col = SPP)) +  geom_boxplot() + 
  labs(x = "Browsing Type", title = "Browsing Preference",
       y = "Ratio of Browsed to Available") + 
  scale_color_manual(name = "Species",
                    labels = c("Alder", 
                               "Paper Birch", "Black Spruce",
                               "Poplar", "Willow"),
                    values = c("#b2df8a",  
                               "#1f78b4", "#fb9a99",
                               "#33a02c", "#e31a1c"))
save_plot("b_pref.png", b_pref, base_aspect_ratio = 1.5)
