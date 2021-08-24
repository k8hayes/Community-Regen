# Species Coverage data
# 11/5/18
# 4/1/19 - adding steese

# setting up 
library(ggplot2)
library(cowplot)

rm(list = ls())
dev.off()
getwd()

#loading in files
  # species coverage
    species_coverage <- read.csv("data/Dalton_Species_coverage.csv")
    steese_coverage <- read.csv("data/Steese_Cover.csv")
  
  # basal area
    basal_area <- read.csv("data/dalton_regen_basal_area.csv")
  # herbivory
    herbivory <- read.csv("data/dalton_regen_herb.csv")
  # organic depth
    org_depth <- read.csv("data/Org_Depth.csv")
  # total regen
    total_regen <- read.csv("data/total_regen.csv")

# compiling averages by variables
site_means <- aggregate(species_coverage$ORGANIC ~ species_coverage$SITENUM,species_coverage, mean)
colnames(site_means) <- c("sitenum", "org_mean")
site_means$treat <- aggregate(species_coverage$TREATMENT ~ species_coverage$SITENUM, species_coverage, max)[,2]

  # ground cover
  site_means$min_mean <- aggregate(species_coverage$MINERAL ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  site_means$rock_mean <- aggregate(species_coverage$ROCK ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  site_means$cwd_mean <- aggregate(species_coverage$CWD ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  site_means$litter_mean <- aggregate(species_coverage$LITTER ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  
  # functional groups
  site_means$lichen_mean <- aggregate(species_coverage$LICHEN_SUM ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  site_means$gram_mean <- aggregate(species_coverage$GRAM_SUM ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  site_means$moss_mean <- aggregate(species_coverage$MOSS_SUM ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  site_means$forb_mean <- aggregate(species_coverage$FORB_SUM ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  site_means$seedvasc_mean <- aggregate(species_coverage$SEEDLVASC_SUM ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  site_means$seedvasc_mean <- aggregate(species_coverage$SEEDLVASC_SUM ~ species_coverage$SITENUM, species_coverage, mean)[,2]
  
  # adding regen and basal area
  site_means$regen_ha <- aggregate(herbivory$TOTAL_HA ~ herbivory$SITENUM, herbivory, mean)[,2]
  site_means$basal_regen_ha <- aggregate(basal_area$BASAL_PHA ~ basal_area$SITENUM, basal_area, mean)[,2]
  
  # splitting by conifer/deciduous
  conifer_regen <- herbivory[(herbivory$DIVISION == "c"),]
  decid_regen <- herbivory[(herbivory$DIVISION == "d"),]
  # adding division
  site_means$decid_regen <- aggregate(decid_regen$TOTAL_HA ~ decid_regen$SITENUM, decid_regen, mean)[,2]
  site_means$conifer_regen <- aggregate(conifer_regen$TOTAL_HA ~ conifer_regen$SITENUM, conifer_regen, mean)[,2]
  
  # adding species division
  bene_regen <- herbivory[herbivory$SPECIES == "BENE",]
  pime_regen <- herbivory[herbivory$SPECIES == "PIME",]
  
  
  
  # organic depth
  dalton_org_depth <- org_depth[(org_depth$SITE == "DALTON"),]
  steese_organic_depth <- org_depth[(org_depth$SITE == "STEESE"),]
  site_means$org_depth <- aggregate(dalton_org_depth$ORG_DEPTH ~ dalton_org_depth$SITENUM, dalton_org_depth, mean)[,2]
  
  # total regen
  

# proportion browsed
# have to subset data since we didn't count browse at 32_2 and 39_2
  herbivory$proportion_brow <- herbivory$BROWSE_HA/herbivory$TOTAL_HA
  site_means$proportion_brow <- c("NA", "NA", aggregate(herbivory$proportion_brow ~ herbivory$SITENUM, herbivory, mean)[,2])
  site_means$proportion_brow <- as.numeric(site_means$proportion_brow)

#########################
# adding steese
  steese_coverage <- read.csv(file = "data/Steese_Cover.csv")
  exposed_min <- steese_coverage[1:7]
  exposed_min_dalton <- species_coverage[1:7]
  colnames(exposed_min_dalton)<- colnames(exposed_min)
  exposed_min <- rbind(exposed_min, exposed_min_dalton)
  rm(exposed_min_dalton)
  write.csv(exposed_min, "exposed_min.csv")
  
# organic depth
  org_depth_plot <- ggplot(org_depth, aes(x = factor(org_depth$TREATMENT), y = org_depth$ORG_DEPTH, fill = org_depth$SITE)) + 
    geom_boxplot() + 
    scale_fill_manual(name = "Site Type",
                      values = c("#d95f0e", "#fec44f"), 
                      labels = c("Upland", "Lowland")) + 
    labs(title = "Organic Layer Depth", 
         x = "Number of Fires", y = "Organic layer depth (cm)") + 
    theme(plot.title = element_text(hjust = 0))
  save_plot("org_depth.png", org_depth_plot, base_aspect_ratio = 1.5)
  
# graminoid cover
  steese_coverage <- read.csv(file = "data/Steese_Cover.csv")
  gram_cover <- steese_coverage[1:10]
  gram_cover_dalton <- species_coverage[1:10]
  colnames(gram_cover_dalton)<- colnames(gram_cover)
  gram_cover <- rbind(gram_cover, gram_cover_dalton)
  gram_cover <- gram_cover[complete.cases(gram_cover),]
  # gram_cover <- gram_cover[-5]
  rm(gram_cover_dalton)
  #write.csv(gram_cover, "gram_cover.csv")
  gram_cover <- read.csv("gram_cover.csv")
  gram_cover_plot <- ggplot(gram_cover, aes(x = as.factor(gram_cover$TREATMENT), y = gram_cover$GRAM, fill = gram_cover$SITE)) + 
    geom_boxplot() + background_grid() + panel_border() +
    labs(title = "Graminoid Cover",
         x = "Number of Fires", y = "% cover")  +
    scale_fill_manual(name = "Site Type",
                      values = c("#bdbdbd", "#636363"),
                      labels = c("Upland", "Lowland")) + theme(plot.title = element_text(hjust = 0))  
  gram_cover_plot
  save_plot("gram_cover.png", gram_cover_plot, base_aspect_ratio = 1.5)

  mean(gram_cover$GRAM[gram_cover$SITE == "STEESE" & gram_cover$TREATMENT == 3])
  mean(gram_cover$GRAM[gram_cover$SITE == "DALTON" & gram_cover$TREATMENT == 3])
  
#plotting exposed mineral soil
 exposed_min <-  read.csv("data/exposed_min.csv")
 exposed_min_plot <-  ggplot(exposed_min, aes(x = as.factor(exposed_min$TREATMENT), y = exposed_min$MINERAL, fill = exposed_min$SITE)) + 
    geom_boxplot() + background_grid() + panel_border() +
    labs(title = "Exposed Mineral Soil",
           x = "Number of Fires", y = "% cover") + 
    scale_fill_manual(name = "Site Type",
                      values = c("#dfc27d", "#018571"),
                      labels = c("Lowland", "Upland")) + theme(plot.title = element_text(hjust = 0))  
save_plot("exposed_min.png", exposed_min_plot, base_aspect_ratio = 1.5)
  
substrate_column_plot <- plot_grid(exposed_min_plot, org_depth_plot, rel_widths = c(1,1), rel_heights = c(1,1), 
          ncol = 1, labels = c("A.", "B."), align = "v")  
save_plot("substrate.png", substrate_column_plot, base_aspect_ratio = 1)
  
######################################################
# PLOTTING
# regen_org <- ggplot(site_means, aes(x = org_mean, y = regen_ha)) + geom_line(aes(color = factor(treat))) + geom_point(aes(color = factor(treat))) + scale_color_brewer(name = "Treatment", palette = "Greys") + labs(title = "Regen according to Organic layer cover", x = "% cover of Organic layer", y = "Average Regeneration per ha") +  geom_smooth(method=lm, se = FALSE)
# regen_min <- ggplot(site_means, aes(x = min_mean, y = regen_ha)) +  geom_line(aes(color = factor(treat))) + geom_point(aes(color = factor(treat)))  +scale_color_brewer(name = "Treatment", palette = "Greys") + labs(title = "Regen. according to Mineral soil cover",x = "% cover of Mineral Soil", y = "Average Regeneration per ha") +  geom_smooth(method=lm, se = FALSE)
# regen_min_org <- plot_grid(regen_org, regen_min, rel_widths = c(1,1), rel_heights = c(1,1), labels = c("A.", "B."))
# save_plot("regen_min_org.png", regen_min_org, ncol= 2, nrow =1, base_aspect_ratio = 1.3)

regen_gram <- ggplot(site_means, aes(x = gram_mean, y = regen_ha)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Regen. according to Grass cover",
        x = "% cover of Grasses", 
       y = "Average Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, col = "black")

regen_moss <- ggplot(site_means, aes(x = moss_mean, y = regen_ha)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Regen. according to Moss cover",
       x = "% cover of Mosses", 
       y = "Average Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE, col = "black")

ggplot(species_coverage, aes(x = factor(species_coverage$TREATMENT), y =species_coverage$MINERAL)) +
  geom_boxplot() + 
  labs(title = "Exposed Mineral Soil",
       x = "Treatment",
       y = "% Cover") + 
  theme(plot.title = element_text(hjust = 0.5))
ggplot(species_coverage, aes(x = factor(species_coverage$TREATMENT), y = species_coverage$MOSS_SUM)) + 
  geom_boxplot() + labs(title = "Moss cover",
                        x = "Treatment",
                        y = "% Cover")


regen_moss

regen_lichen <- ggplot(site_means, aes(x = lichen_mean, y = regen_ha)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "Greys") + 
  labs(title = "Regen. according to Lichen cover",
       x = "% cover of Lichen", 
       y = "Average Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)
regen_moss_lichen <- plot_grid(regen_moss, regen_lichen,rel_widths = c(1,1), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("regen_moss_lichen.png", regen_moss_lichen, ncol= 2, nrow =1, base_aspect_ratio = 1.3)

# polytrichum
ggplot(site_means, aes(x = , y = regen_ha)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "Greys") + 
  labs(title = "Regen. according to Lichen cover",
       x = "% cover of Lichen", 
       y = "Average Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)

##############################################################################
# PLOTTING CONIFER VS DECIDUOUS
# moss
decid_moss <- ggplot(site_means, aes(x = moss_mean, y = decid_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "Greys") + 
  labs(title  = "Deciduous",
       x = "% cover of Mosses", 
       y = "Av. Deciduous Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)

conifer_moss <- ggplot(site_means, aes(x = moss_mean, y = conifer_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "Greys") + 
  labs(title = "Conifers",
       x = "% cover of Mosses", 
       y = "Av. Conifer Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)
decid_con_moss <- plot_grid(decid_moss, conifer_moss, rel_widths = c(1,1), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("decid_con_moss.png", decid_con_moss, ncol= 2, nrow =1, base_aspect_ratio = 1.3)

# grass
decid_gram <- ggplot(site_means, aes(x = gram_mean, y = decid_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Deciduous",
       x = "% cover of Grasses", 
       y = "Av. Deciduous Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)

conifer_gram <- ggplot(site_means, aes(x = gram_mean, y = conifer_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Conifers",
       x = "% cover of Grasses", 
       y = "Av. Conifer Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)
decid_con_gram <- plot_grid(decid_gram, conifer_gram, rel_widths = c(1,1), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("decid_con_gram.png", decid_con_gram, ncol= 2, nrow =1, base_aspect_ratio = 1.3)

# org soil
decid_org <- ggplot(site_means, aes(x = site_means$org_mean, y = decid_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "Greys") + 
  labs(title = "Deciduous",
       x = "% cover of Organic Soils", 
       y = "Av. Deciduous Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)

conifer_org <- ggplot(site_means, aes(x = site_means$org_mean, y = conifer_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "Greys") + 
  labs(title = "Conifers",
       x = "% cover of Organic Soils", 
       y = "Av. Conifer Regeneration per ha") + geom_smooth(method=lm, se = FALSE, fullrange = TRUE)
plot_decid_con_org <- plot_grid(decid_org, conifer_org, rel_widths = c(1,1), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("decid_con_org.png", plot_decid_con_org, ncol= 2, nrow =1, base_aspect_ratio = 1.3)

# mineral soils
decid_min <- ggplot(site_means, aes(x = site_means$min_mean, y = decid_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Deciduous",
       x = "% cover of Mineral Soils", 
       y = "Av. Deciduous Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)

conifer_min <- ggplot(site_means, aes(x = site_means$min_mean, y = conifer_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  panel_border() + background_grid() +
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Conifers",
       x = "% cover of mineral Soils", 
       y = "Av. Conifer Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)
plot_decid_con_min <- plot_grid(decid_min, conifer_min, rel_widths = c(1,1), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("decid_con_min.png", plot_decid_con_min, ncol= 2, nrow =1, base_aspect_ratio = 1.3)

## herbivory
decid_herb <- ggplot(site_means, aes(x = site_means$proportion_brow, y = decid_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat)))  +
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Deciduous",
       x = "% Browsed", 
       y = "Av. Deciduous Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE, col = "black")
decid_herb

conifer_herb <- ggplot(site_means, aes(x = site_means$proportion_brow, y = conifer_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Conifers",
       x = "% Browsed", 
       y = "Av. Conifer Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE, col = "black")

ggplot(site_means, aes(x = site_means$treatment, y = conifer_regen)) + 
  geom_boxplot(aes(color = factor(treat))) + 
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Conifers",
       x = "% Browsed", 
       y = "Av. Conifer Regeneration per ha") 

conifer_herb
plot_decid_con_herb <- plot_grid(decid_herb, conifer_herb, rel_widths = c(1,1), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("decid_con_herb.png", plot_decid_con_herb, ncol= 2, nrow =1, base_aspect_ratio = 1.3)





# organic depth
decid_org_depth <- ggplot(site_means, aes(x = site_means$org_depth, y = decid_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Deciduous",
       x = "Av. organic layer depth (cm)", 
       y = "Av. Deciduous Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE, col = "black")
decid_org_depth

conifer_org_depth <- ggplot(site_means, aes(x = site_means$org_depth, y = conifer_regen)) + 
  geom_line(aes(color = factor(treat))) + 
  geom_point(aes(color = factor(treat))) + 
  scale_color_brewer(name = "Treatment",
                     palette = "YlOrRd") + 
  labs(title = "Conifers",
       x = "Av. organic layer depth (cm)", 
       y = "Av. Conifer Regeneration per ha") +
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE, col = "black")
conifer_org_depth
decid_con_org_depth <- plot_grid(decid_org_depth, conifer_org_depth, rel_widths = c(1,1), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("decid_con_org_depth.png", decid_con_org_depth, ncol= 2, nrow =1, base_aspect_ratio = 1.3)

#########################################################
# boxplots
ggplot(org_depth, aes(x = factor(org_depth$TREATMENT), y = org_depth$ORG_DEPTH, fill = org_depth$SITE)) + 
  geom_boxplot() + 
  labs(title = "Organic layer depth across treatment", 
       x = "Treatment", y = "Organic layer depth (cm)",
       fill = "Site")

ggplot(species_coverage, aes(x = factor(species_coverage$TREATMENT), y = species_coverage$GRAM_SUM)) + 
  geom_boxplot(fill = "#a1d99b") +
  labs(x = "Treatment", y = "% cover of grasses",
       title = "Grass cover across treatments")

ggplot(species_coverage, aes(x = factor(species_coverage$TREATMENT), y = species_coverage$MOSS_SUM)) + 
  geom_boxplot(fill = "#a6bddb") + 
  labs(x = "Treatment", y = "% cover of mosses",
       title = "Moss cover across treatments")


ggplot(herbivory, aes(x = factor(herbivory$TREAT), y = log(herbivory$TOTAL_HA))) + 
  geom_boxplot(aes(fill = herbivory$DIVISION)) )+
  scale_fill_manual(labels = c("Conifers", "Deciduous"), 
                    values = c("#ffeda0", "#d95f0e")) + 
  labs(title = "Overall Regeneration",
       x = "Treatment", y = "Regeneration per ha",
       fill = "Divison")

################################
# splitting by species







