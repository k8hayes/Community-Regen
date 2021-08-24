# Calculating Bray-Curtis dissimilarity index

install.packages("vegan")
library(tidyverse)
library(vegan)
library(here)

cover <- read.csv(here("data/Dalton_Sp_cover.csv"))

unique(cover$SITE.CODE)
length(unique(cover$SITE.CODE))

colnames(cover)

cover <- cover %>%
  rename("PLOT" = "SITE.CODE", "TREAT" = "TREATMENT") %>%
  select(!c(SITE, SITENUM))

write.csv(cover, here("data/Dalton_Sp_cover.csv"), row.names = FALSE)
