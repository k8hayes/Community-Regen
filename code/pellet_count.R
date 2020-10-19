# pellet counts

library(tidyverse)
library(ggplot2)
library(cowplot)
library(here)
theme_set(theme_cowplot())

pellet <- read.csv(here("pellet_counts_2019.csv"))
head(pellet)

pellet <- pellet %>%
  rename(spp_hare = HARE , spp_moose = MOOSE,
         present_hare = HARE_PRESENT, present_moose = MOOSE_PRESENT)
pellet %>%
  filter(TREAT !=0) %>%
  filter(SITE != "DALTON") %>%
  summarize(median(spp_hare))
  
pellet_piv <- pellet %>%
  pivot_longer(cols = starts_with("spp"), 
    names_to = "SPP", 
    values_to = "pellet")
pell_piv_pres <- pellet %>%
  pivot_longer(cols = starts_with("present"),
               names_to = "SPP",
               values_to = "Y_N")

pres <- pellet %>%
  group_by(SITENUM) %>%
  distinct(present_hare, present_moose)

hare <-  ggplot(pellet, aes(x = as.factor(TREAT), y = HARE, fill = SITE)) + 
  geom_boxplot() + ylim(0,100) +
   scale_fill_manual(name = "Site",
                     labels = c("Upland", "Lowland"),
                     values = c("azure3", "azure4")) + 
   theme(plot.title = element_text(hjust = 0)) +
  labs(title = "Hare Pellet counts", x = "Number of fires",
       y = "Pellets per square meter")
save_plot( "hare.png",hare, base_aspect_ratio = 1.5)
 
ggplot(pellet, aes(x = as.factor(TREAT), y = MOOSE, fill = SITE)) + 
  geom_boxplot() + ylim(0,20) +
  panel_border() + background_grid() +
  theme(plot.title = element_text(hjust = 0)) +
  labs(title = "Moose Pellet counts", x = "Number of fires",
       y = "Pellets per square meter")

dalt_pell <- pellet_piv %>%
  filter(pellet_piv$SITE != "STEESE") %>%
  ggplot(aes(x = as.factor(TREAT), y = pellet, fill = SPP)) + 
  geom_boxplot() +
  scale_fill_manual(name = "Browse Type",
                    labels = c("Hare", "Moose"),
                    values = c("chocolate", "brown")) + 
  labs(title = "Upland Pellet Counts", x = "Number of Fires",
       y = "Pellet Counts (per 100m2)" ) + theme(legend.position = "none")
dalt_pell
ste_pell <- pellet_piv %>%
  filter(pellet_piv$SITE != "DALTON") %>%
  ggplot(aes(x = as.factor(TREAT), y = pellet, fill = SPP)) + 
  geom_boxplot() +
  scale_fill_manual(name = "Browse Type",
                    labels = c("Hare", "Moose"),
                    values = c("chocolate", "brown")) + 
  labs(title = "Lowland Pellet Counts", x = "Number of Fires",
       y = " " )
ste_pell
site_pell <- plot_grid(dalt_pell, ste_pell, nrow = 1, 
                       rel_widths = c(1,1.5), labels = c("A."))
site_pell; rm(ste_pell, dalt_pell)
# save_plot("site_pell.png", site_pell) # save as png # 650 by 300

pell_piv_pres %>%
  filter(pell_piv_pres$SPP != "spp_hare") %>%
  ggplot(aes(x = as.factor(TREAT))) + 
    geom_bar(aes(fill = Y_N), position = "dodge2") + facet_wrap(~SITE)

       