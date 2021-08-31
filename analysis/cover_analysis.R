# Calculating Bray-Curtis dissimilarity index

library(tidyverse)
library(vegan)
library(here)
library(cowplot)
theme_set(theme_cowplot())

# Clean up ######
      # cover <- read.csv(here("data/Dalton_Sp_cover.csv"))
      #
      # unique(cover$SITE.CODE)
      # length(unique(cover$SITE.CODE))
      #
      # colnames(cover)
      #
      # cover <- cover %>%
      #   rename("PLOT" = "SITE.CODE", "TREAT" = "TREATMENT") %>%
      #   select(!c(SITE, SITENUM))
      #
      # write.csv(cover, here("data/Dalton_Sp_cover.csv"), row.names = FALSE)

# Looking for unique spp #########

cover <- read.csv(here("data/Dalton_Sp_cover.csv"))

unique(cover$TREAT[cover$CLCH >0])


# Plotting trends #########

ggplot(cover, aes(x = as.factor(TREAT), y = MOSS_SUM)) + geom_boxplot()

ggplot(cover, aes(x = as.factor(TREAT), y = LICHEN_SUM)) + geom_boxplot()

ggplot(cover, aes(x = as.factor(TREAT), y = GRAM_SUM)) + geom_boxplot()

ggplot(cover, aes(x = as.factor(TREAT), y = FORB_SUM)) + geom_boxplot()



# test #####

plot10 <-cover %>%
   filter(PLOT == "32_2")

test <- cover %>%
   select(!c(DECID_TREE_SUM,DECID_TOTAL, DECID_SHRUB_SUM,
             LICHEN_SUM, MOSS_SUM, FORB_SUM, GRAM_SUM,
             EVERGREEN_SHRUB, SEEDLVASC_SUM, CONIFER_SUM)) %>%
   pivot_longer(!c(PLOT, CORNER, TREAT),
                names_to = "Species", values_to = "cover")

test <- test %>%
   group_by(PLOT, TREAT, Species) %>%
   summarise(coverAV = mean(cover), coverSD = sd(cover))


ggplot(test, aes(x = as.factor(TREAT), y = coverAV)) +
   geom_point() + geom_jitter()

# ADD GENUS COLUMN
