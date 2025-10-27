library(tidyverse)
library(leaps)

understory = read.csv("data/understory.csv")
env <- read.csv("data/site_attributes.csv")
soil <- read.csv("data/org_depth.csv")

env <- env %>%
  filter(SITE != "STEESE") %>%
  mutate(plot = str_remove(SITECODE, pattern = "\\_.*")) %>%
  dplyr::select(plot, SLOPE, ASPECT_RAW, ELEVATION, SOLAR, TSF)

soil <- soil %>%
  filter(SITE != "STEESE") %>%
  mutate(plot = str_remove(PLOT, pattern = "\\_.*"))

soil_sum = soil %>%
  group_by(plot) %>%
  summarise(avSOL_depth = mean(ORG_DEPTH_cm)) %>%
  ungroup() %>%
  dplyr::select(plot, avSOL_depth)

summary(soil_sum)
summary(understory)
understory = understory %>%
  mutate(plot = as.character(plot))

summary(env)

mech = full_join(soil_sum, understory, join_by(plot))
mech = full_join(mech, env, join_by(plot))

#####################

density = read.csv("/Users/katherinehayes/Google Drive/Projects/NSF Reburns/Chapters/Tree Regen/data/dbh.csv")

density = density %>%
  filter(SITE == "DALTON") %>%
  mutate(live_dead = ifelse(CANOPY == 0, "Dead", "Live"))

density_count = density %>%
  group_by(PLOT,live_dead) %>%
  mutate(count_ha = n()*EXP_FACT) %>%
  ungroup() %>%
  complete(nesting(PLOT), live_dead, fill = list(count_ha = 0)) %>%
  group_by(PLOT, live_dead) %>%
  distinct(count_ha)

dens = density_count %>%
  pivot_wider(names_from = live_dead, values_from = count_ha) %>%
  rename("dens_ha_dead" = "Dead",  "dens_ha_live" = "Live") %>%
  mutate(plot = str_remove(PLOT, pattern = "\\_.*")) %>%
  ungroup() %>%
  dplyr::select(!PLOT)

mech = full_join(mech, dens, join_by(plot))

######################################

richness = mech %>%
  dplyr::select(!simpson) %>%
  dplyr::select(!plot) %>%
  filter(treatment != 0)

test = regsubsets(Richness ~ ., richness, nvmax = 12)
result_base = summary(test)
result_base

test_forward = regsubsets(Richness ~ ., richness, nvmax = 12,
                          method = "forward")

test_backward = regsubsets(Richness ~ ., richness, nvmax = 12,
                           method = "backward")


result_forward = summary(test_forward)
result_back = summary(test_backward)

test_forward


tibble(predictors = 1:12,
       adj_R2 = result_base$adjr2,
       Cp = result_base$cp,
       BIC = result_base$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  facet_wrap(~ statistic, scales = "free")
