# processing herbivory data
library(tidyverse)
library(here)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

herb <- read.csv(here("Herb_raw.csv"), stringsAsFactors = F)

herb <- subset(herb, select = -c(X, X.1))
# adding divisions
  unique(herb$SPP)
  herb$DIV[herb$SPP == "BENE"] <- "d"
  herb$DIV[herb$SPP == "ALCR"] <- "d"
  herb$DIV[herb$SPP == "POTR"] <- "d"
  herb$DIV[herb$SPP == "SALIX"] <- "d"
  herb$DIV[herb$SPP == "PIME"] <- "c"
  herb$DIV[herb$SPP == "PIGL"] <- "c"
  herb$DIV[herb$SPP == "ARCTO"] <- "d"
  herb$DIV[herb$SPP == "POBA"] <- "d"
  
# UNITS
  # transforming height from feet to meters
    herb$HEIGHT_M <- herb$HEIGHT_FT/3.281
    herb$HEIGHT_M <- round(herb$HEIGHT_M, digits = 1)
  # dbh from inches to cm
    herb$DBH_CM <- herb$DBH_INCH *2.54
    herb$DBH_CM <- round(herb$DBH_CM, digits = 2)

    write.csv(herb, "herb.csv", row.names = F)