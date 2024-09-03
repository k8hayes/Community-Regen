## analysis done in R version 4.1.2

setwd("~/Carter/Research/Papers/In Progress/Understory Reburns AK/data") ## set working director to data folder

## all dependencies required to run the following analysis 
library(tidyr)
library(vegan)
library(dplyr)
library(MASS)
library(pwr)
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=0, length=length, ...)
}## creating a function to add arrows on the chart
se <- function(x){sd(x)/sqrt(length(x))} ## creating a function for standard error
pseudo.R.squared <- function(model){
  1 - (model$deviance/model$null.deviance)
} ## R squared function 


#### Data Cleaning ####
understory <- read.csv("Upland_UndCover.csv") ## reading in the data
understory$ORGANIC <- NULL ## removing non-species data
understory$MINERAL <- NULL
understory$ROCK <- NULL
understory$CWD <- NULL
understory$LITTER <- NULL
table(colnames(understory)) ## visualizing the species present. 

understory <- understory[-grep("UNK", colnames(understory))] ## removing unknowns from main understory data

understory_plots <- data.frame(plot = unique(understory$PLOT)) ## making a dataframe with only plots
understory_plots$treat <- understory$TREAT[match(understory_plots$plot, understory$PLOT)] ## and treatments to reference later

understory_long <- gather(understory,species,cover, Achillea.millefolium:Vaccinium.viginosum) ## converting to long format to summarize

understory_long <- understory_long %>% 
  group_by(PLOT, species) %>% 
  summarise(plotavg = mean(cover)) ## summarizing by transect then by plot; taking the average cover 1m squared
understory_long <- as.data.frame(understory_long) ## converting the tibble to a data frame
max(understory_long$plotavg) ## looking at the maximum species cover for a plot; nothing too crazy
min(understory_long$plotavg) ## looking at the minimum species cover for a plot
length(unique(understory_long$species)) ## currently 72 unique species
length(unique(understory_long$PLOT)) ## 26 plots
length(unique(understory$PLOT)) ## making sure the number of plots match; they should both be 26

## Functional Groups Adding ##
FGkey <- read.csv("Understory_FG.csv") ## reading in the functional group key
FGkey$Species <- sub(" ", ".", FGkey$Species) ## changing the species column to read the same as species in the understory dataframe

understory_long$FG <- FGkey$Type[match(understory_long$species, FGkey$Species)]
unique(understory_long$species[is.na(understory_long$FG)])
## a few species not covered in the FGkey
# Achillea.millefolium == forb
# Alnus.virdis == decid shrub
# Cetraria.culculata == Lichen
# Populus.tremuloides == deciduous trees
# Ptilium.crista.castrensis == Moss
# Salix == decid shrub

FGkey.addendum <- data.frame(Type = c("forb", "decid shrub", "Lichen",
                                      "deciduous trees", "Moss", "decid shrub"),
                             acronym = NA,
                             Genus = NA,
                             Species = unique(understory_long$species[is.na(understory_long$FG)]),
                             Notes = NA)
FGkey <- rbind(FGkey, FGkey.addendum);rm(FGkey.addendum)

understory_long$FG <- FGkey$Type[match(understory_long$species, FGkey$Species)] ## adding final functional groups
unique(understory_long$species[is.na(understory_long$FG)]) ## no NAs so all species have an associated Functional Group


length(unique(understory_long$species)) ## currently 72 unique species, but includes overstory species
table(understory_long$FG) ## looking at the functional groups; per 1/17/2024 decided to lump FG to be a bit more common 

understory_long$FG[understory_long$FG == "conifers" | understory_long$FG == "deciduous trees"] <- "tree" ## lumping a tree group
understory_long$FG[understory_long$FG == "decid shrub" | understory_long$FG == "evergreen shrub"] <- "shrub" ## lumping a shrub group
# understory_long$FG[understory_long$FG == "Lichen" | understory_long$FG == "Moss"] <- "non-vascular" ## lumping a non-vascular group

understory_long <- understory_long[understory_long$FG != "tree",] ## removing tree species
length(unique(understory_long$species)) ## currently 67 unique species

understory_wide <- reshape(understory_long[,c(1:3)], idvar = "PLOT", timevar = "species", direction = "wide") ## reshaping the data
rownames(understory_wide) <- understory_wide$PLOT ## renaming rows to be PLOT
understory_wide$PLOT <- NULL ## removing nonspecies column
colnames(understory_wide) <- sub("plotavg.", "", colnames(understory_wide)) ## changing column names to be species codes

table(colnames(understory_wide)) ## looking at the species, will correct the ones I need to

understory <- as.data.frame(apply(understory_wide, 1, sum, na.rm = TRUE)) ## getting the total cover per plot
colnames(understory) <- "TotalPlotCover" ## renaming the column for later
tmp <- as.data.frame(ifelse(understory_wide[,] > 0,1,0)) ## creating a Presence/Absence Richness Dataframe
understory$Richness <- apply(tmp, 1, sum, na.rm = TRUE);rm(tmp) ## getting the count of each species
understory$simpson <- diversity(understory_wide, index = "simpson") ## calculating simpson diversity index
understory$treatment <- as.factor(understory_plots$treat[match(rownames(understory), understory_plots$plot)])

hist(understory$TotalPlotCover) 
hist(understory$Richness)
hist(understory$simpson)

rm(FGkey)

#### Figure 2 - Ordination and Bray-Curtis ####
pal <- c("#f6d746", "#e55c30", "#84206b", "#140b34")
pal2 <- c(rgb(0.96,0.84,0.27,0.5),rgb(0.89,0.36,0.18,0.5),rgb(0.51,0.12,0.41,0.5),rgb(0.07,0.04,0.20,0.5))

## NMDS Ordination
par(mfrow = c(1,2))
set.seed(1)
NMDSord <- metaMDS(understory_wide, try = 1000, distance = "bray") ## convergence

env <- data.frame(site = rownames(understory),
                  treatment = understory$treatment)
rownames(env) <- env$site
str(env)

plot(NMDSord, type = "n",display = "sites", las = 1) 
points(NMDSord, display = "sites", pch = 19, cex = .75, col = pal[as.factor(env$treatment)])
# text(NMDSord, display = "species", cex = 1) ## text to add plant codes
# vectors <- envfit(NMDSord, env$treatment)
# plot(vectors, col = "firebrick", type = "n")

ordiellipse(NMDSord, display = "sites", env$treatment, draw = "lines",
            col = pal, label = FALSE) ## ellipses based on slide exposure
legend("bottomright", legend = c("No Fire", "1 Fire", "2 Fires", "3 Fires"), col = pal, pch = 15, cex = 1, ncol = 2, bty = "n")

## Bray-Curtis Dissimilarity analysis
understory_FireMat <- understory_wide ## making a second matrix
understory_plots$trt_site <- paste(understory_plots$treat, understory_plots$plot, sep = "_") ## creating a unique treatment
rownames(understory_FireMat) <- understory_plots$trt_site[match(rownames(understory_wide), understory_plots$plot)]
understory_FireMat <- understory_FireMat[order(rownames(understory_FireMat)),]

understory_FireMat <- as.matrix(understory_FireMat) ## turning community dataframe into a matrix
Understory.dist <- vegdist(understory_FireMat, method ="bray", na.rm = TRUE) ## creating a dissimilarity matrix for Bray-Curtis
Understory.dist.mat <- as.matrix(Understory.dist) ## turning into a matrix

T0.mat <- Understory.dist.mat[grep("0_", rownames(Understory.dist.mat)),grep("0_", colnames(Understory.dist.mat))]
mean(T0.mat)
T0.vec <- c(T0.mat)
mean(T0.vec)
T0.vec <- unique(T0.vec)

T1.mat <- Understory.dist.mat[grep("1_", rownames(Understory.dist.mat)),grep("1_", colnames(Understory.dist.mat))]
mean(T1.mat)
T1.vec <- c(T1.mat)
mean(T1.vec)
T1.vec <- unique(T1.vec)

T2.mat <- Understory.dist.mat[grep("2_", rownames(Understory.dist.mat)),grep("2_", colnames(Understory.dist.mat))]
mean(T2.mat)
T2.vec <- c(T2.mat)
mean(T2.vec)
T2.vec <- unique(T2.vec)

T3.mat <- Understory.dist.mat[grep("3_", rownames(Understory.dist.mat)),grep("3_", colnames(Understory.dist.mat))]
mean(T3.mat)
T3.vec <- c(T3.mat)
mean(T3.vec)
T3.vec <- unique(T3.vec)

disimAOV <- data.frame(vals = c(T0.vec, T1.vec, T2.vec, T3.vec),
                       treat = c(rep("T0", length(T0.vec)), rep("T1", length(T1.vec)),rep("T2", length(T2.vec)),rep("T3", length(T3.vec))))
str(disimAOV)
disimAOV$treat <- as.factor(disimAOV$treat)
disimAOV <- disimAOV[disimAOV$vals != 0,] ## removing comparisons to self
a1 <- aov(vals ~ treat, data = disimAOV)
summary(a1)
TukeyHSD(a1)
plot(vals~treat,
     data = disimAOV,
     col = pal2,
     las = 1,
     ylim = c(0,1),
     xlab = "Number of Fires",
     names = c("0", "1", "2", "3"),
     ylab = "Bray-Curtis Dissimilarity")
axis(3,at = 1:4, labels = c("A", "B", "C", "BC"))
stripchart(vals~treat, data=disimAOV, 
           vertical = TRUE, 
           col = "black",
           method = "jitter", 
           add = TRUE, 
           pch = 20, 
           cex = 0.75)

#### Summary of Results for Figure 2 ####
aggregate(disimAOV$vals, by = list(disimAOV$treat), FUN = mean)
# Mean         x
# 1      T0 0.1922171
# 2      T1 0.5714884
# 3      T2 0.6753622
# 4      T3 0.5804762

aggregate(disimAOV$vals, by = list(disimAOV$treat), FUN = se)[,2]*1.96 ## 95% CI
# 95% CI        x
# 1      T0 0.04686374 
# 2      T1 0.03433148 
# 3      T2 0.05782254 
# 4      T3 0.05193598

summary(a1)
TukeyHSD(a1)

rm(disimAOV);rm(env);rm(T0.mat);rm(T1.mat);rm(T2.mat);rm(T3.mat);rm(Understory.dist.mat)
rm(T0.vec);rm(T1.vec);rm(T2.vec);rm(T3.vec);rm(Understory.dist)
rm(a1);rm(NMDSord)

#### Figure 3 - Richness and Simpsons ####
a2 <- aov(Richness ~ treatment, data = understory)
summary(a2)
TukeyHSD(a2)
plot(Richness~treatment,
     data = understory,
     col = pal2,
     outline = F,
     las = 1,
     ylim = c(0,40),
     xlab = "Number of Fires",
     ylab = "Number of Species")
axis(3,at = 1:4, labels = c("A", "B", "AC", "BC"))
stripchart(Richness~treatment, data=understory, 
           vertical = TRUE, 
           method = "jitter", 
           add = TRUE, 
           pch = 20, 
           cex = 0.75)

a3 <- aov(simpson ~ treatment, data = understory)
summary(a3)
TukeyHSD(a3)
plot(simpson~treatment,
     data = understory,
     col = pal2,
     las = 1,
     ylim = c(0,1),
     xlab = "Number of Fires",
     ylab = "Simpson Diversity Index")
axis(3,at = 1:4, labels = c("A", "B", "B", "B"))
stripchart(simpson~treatment, data=understory, 
           vertical = TRUE, 
           method = "jitter", 
           add = TRUE, 
           pch = 20, 
           cex = 0.75)

#### Summary of Results for Figure 3 ####
aggregate(understory$Richness, by = list(understory$treatment), FUN = mean)
aggregate(understory$Richness, by = list(understory$treatment), FUN = se)[,2] *1.96 ## confidence interval
summary(a2)
TukeyHSD(a2)

aggregate(understory$simpson, by = list(understory$treatment), FUN = mean)
aggregate(understory$simpson, by = list(understory$treatment), FUN = se)[,2] *1.96 ## confidence interval
summary(a3)
TukeyHSD(a3)

rm(a2);rm(a3)


#### Figure 4 - Rank Abundance Curves ####
layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = T)) 
par(oma = c(13,2.5,1,1))
layout.show(n=4) 

## First panel is for functional groups 
understory_long_FG <- understory_long %>% 
  group_by(PLOT, FG) %>% 
  summarise(plot.tot = sum(plotavg)) ## summarizing by transect then by plot; taking the average cover 1m squared
understory_long_FG <- as.data.frame(understory_long_FG) ## converting the tibble to a data frame

understory_wide_FG <- reshape(understory_long_FG, idvar = "PLOT", timevar = "FG", direction = "wide") ## reshaping the data
rownames(understory_wide_FG) <- understory_wide_FG$PLOT ## renaming rows to be PLOT
understory_wide_FG$PLOT <- NULL ## removing nonspecies column
colnames(understory_wide_FG) <- sub("plot.tot.", "", colnames(understory_wide_FG)) ## changing column names to be species codes

table(colnames(understory_wide_FG)) ## looking at the species, will correct the ones I need to

rownames(understory_wide_FG) <- understory_plots$trt_site[match(rownames(understory_wide_FG), understory_plots$plot)]
understory_wide_FG <- understory_wide_FG[order(rownames(understory_wide_FG)),]

b0 <- as.data.frame(understory_wide_FG[grep("0_", rownames(understory_wide_FG)),])
b1 <- as.data.frame(understory_wide_FG[grep("1_", rownames(understory_wide_FG)),])
b2 <- as.data.frame(understory_wide_FG[grep("2_", rownames(understory_wide_FG)),])
b3 <- as.data.frame(understory_wide_FG[grep("3_", rownames(understory_wide_FG)),])

burn.sum <- data.frame(cov0 = apply(b0,2, mean),
                       cov1 = apply(b1,2, mean),
                       cov2 = apply(b2,2, mean),
                       cov3 = apply(b3,2, mean))

## First panel is a rank abundance curve of the functional groups from the no burns communities
burn.0 <- burn.sum[rev(order(burn.sum$cov0)),]

burn.0 <- t(burn.0)
colnames(burn.0)

st.dev.1 <- data.frame(species = NA, se0 = NA, se1 = NA, se2 = NA, se3 = NA) ## creating a dataframe to hold SE values, variable naming is innacurate, not calculating SD
st.dev.1[1,1] <- "Moss"
st.dev.1[1,2] <- se(b0$Moss)*1.96
st.dev.1[1,3] <- se(b1$Moss)*1.96
st.dev.1[1,4] <- se(b2$Moss)*1.96
st.dev.1[1,5] <- se(b3$Moss)*1.96

st.dev.1[2,1] <- "Shrub"
st.dev.1[2,2] <- se(b0$shrub)*1.96
st.dev.1[2,3] <- se(b1$shrub)*1.96
st.dev.1[2,4] <- se(b2$shrub)*1.96
st.dev.1[2,5] <- se(b3$shrub)*1.96

st.dev.1[3,1] <- "Lichen"
st.dev.1[3,2] <- se(b0$Lichen)*1.96
st.dev.1[3,3] <- se(b1$Lichen)*1.96
st.dev.1[3,4] <- se(b2$Lichen)*1.96
st.dev.1[3,5] <- se(b3$Lichen)*1.96

st.dev.1[4,1] <- "Seedless \n Vascular"
st.dev.1[4,2] <- se(b0$`seedless vascular`) *1.96
st.dev.1[4,3] <- se(b1$`seedless vascular`) *1.96
st.dev.1[4,4] <- se(b2$`seedless vascular`)*1.96
st.dev.1[4,5] <- se(b3$`seedless vascular`) *1.96

st.dev.1[5,1] <- "Forb"
st.dev.1[5,2] <- se(b0$forb)*1.96
st.dev.1[5,3] <- se(b1$forb)*1.96
st.dev.1[5,4] <- se(b2$forb)*1.96
st.dev.1[5,5] <- se(b3$forb)*1.96

st.dev.1[6,1] <- "Graminoid"
st.dev.1[6,2] <- se(b0$graminoid)*1.96
st.dev.1[6,3] <- se(b1$graminoid)*1.96
st.dev.1[6,4] <- se(b2$graminoid)*1.96
st.dev.1[6,5] <- se(b3$graminoid)*1.96

rownames(st.dev.1) <- st.dev.1[,1] ## making species the rownames
st.dev.1 <- as.matrix(st.dev.1[,-1]) ## turning into a matrix without species col
st.dev.1 <- t(st.dev.1) ## transposing the matrix

ze_bar <- barplot(burn.0, 
                  col= pal , 
                  border="black", 
                  font.axis= 4,
                  # cex.names = 1.5,
                  cex.axis = 1.5,
                  names.arg = c("","","","","",""),
                  las = 1,
                  ylim = c(0,100),
                  beside=T, 
                  xlab="", 
                  font.lab=2)
error.bar(ze_bar,burn.0, st.dev.1)

axis(1, pos = -10, at=c(3,8,13,18,23,28), tick = F, cex.axis = 1.5, font = 4, labels=c('Moss','Shrub','Lichen',"Seedless \n Vascular", 'Forb', "Graminoid"))
mtext(side = 2, line = 3.5, cex = 1.5, "Average Cover (%)")
mtext(side = 3, line = .5, cex = 1.5,font = 2, "Functional Groups")
legend("top", 
       legend=c("No Fire","1 Fire","2 Fires","3 Fires"), 
       col=pal, 
       ncol = 4,
       pt.cex=2, 
       text.font = 2,
       cex = 1.5,
       pch=15,
       bty = "n")


## Panels 2-4 are a rank abundance curves of the top 10 species of each burn to show turnover
b0 <- as.data.frame(understory_FireMat[grep("0_", rownames(understory_FireMat)),])
b1 <- as.data.frame(understory_FireMat[grep("1_", rownames(understory_FireMat)),])
b2 <- as.data.frame(understory_FireMat[grep("2_", rownames(understory_FireMat)),])
b3 <- as.data.frame(understory_FireMat[grep("3_", rownames(understory_FireMat)),])

burn.sum <- data.frame(cov0 = apply(b0,2, mean),
                       cov1 = apply(b1,2, mean),
                       cov2 = apply(b2,2, mean),
                       cov3 = apply(b3,2, mean))

burn.0 <- burn.sum[rev(order(burn.sum$cov0)),c(1,2)]
burn.1 <- burn.sum[rev(order(burn.sum$cov1)),c(2,3)]
burn.2 <- burn.sum[rev(order(burn.sum$cov2)),c(3,4)]

top10.0 <- burn.0[1:10,] ## pulling out the top 10 species for each fire turnover
top10.1 <- burn.1[1:10,]
top10.2 <- burn.2[1:10,]

top10.0 <- t(top10.0) ## transposing to format the df properly
top10.1 <- t(top10.1)
top10.2 <- t(top10.2)

colnames(top10.0)
colnames(top10.0) <- c("Hylocomium splendens", 
                     "Vaccinium viginosum" ,    
                     "Ledum groenlandicum" ,
                     "Pleurozoum scherberi",   
                     "Oxycoccus microcarpos" , 
                     "Pelitigera neopolydacta",
                     "Cladonia rangiferina", 
                     "Equisetum scripotes"  , 
                     "Rosa acicularis",
                     "Peltigera apthosa")

colnames(top10.1)
colnames(top10.1) <- c("Ledum decumbens", 
                       "Poa arctea" ,    
                       "Vaccinium viginosum" ,
                       "Equisetum sylvaticum",   
                       "Ceratodon purpureus" , 
                       "Salix spp.",
                       "Polytrichum strictum", 
                       "Cladonia deformis"  , 
                       "Ledum groenlandicum",
                       "Luzula arcuata")

colnames(top10.2)
colnames(top10.2) <- c("Equisetum sylvaticum", 
                       "Ceratodon purpureus" ,    
                       "Poa pratensis" ,
                       "Vaccinium viginosum",   
                       "Vaccinium vulcanorum" , 
                       "Equisetum arvense",
                       "Polytrichum commune", 
                       "Polytrichum strictum"  , 
                       "Dicranium antifolium",
                       "Salix spp.")

## unburned
st.dev.0 <- data.frame(species = NA, se0 = NA, se1 = NA) ## creating a dataframe to hold SE values, variable naming is innacurate, not calculating SD
st.dev.0[1,1] <- "Hylocomium splendens" ## species name
st.dev.0[1,2] <- se(b0$Hylocomium.splendens)*1.96
st.dev.0[1,3] <- se(b1$Hylocomium.splendens)*1.96

st.dev.0[2,1] <- "Vaccinium viginosum"
st.dev.0[2,2] <- se(b0$Vaccinium.viginosum)*1.96
st.dev.0[2,3] <- se(b1$Vaccinium.viginosum)*1.96

st.dev.0[3,1] <- "Ledum groenlandicum"
st.dev.0[3,2] <- se(b0$Ledum.groenlandicum)*1.96
st.dev.0[3,3] <- se(b1$Ledum.groenlandicum)*1.96

st.dev.0[4,1] <- "Pleurozoum scherberi"
st.dev.0[4,2] <- se(b0$Pleurozoum.scherberi)*1.96
st.dev.0[4,3] <- se(b1$Pleurozoum.scherberi)*1.96

st.dev.0[5,1] <- "Oxycoccus microcarpos"
st.dev.0[5,2] <- se(b0$Oxycoccus.microcarpos)*1.96
st.dev.0[5,3] <- se(b1$Oxycoccus.microcarpos)*1.96

st.dev.0[6,1] <- "Pelitigera neopolydacta"
st.dev.0[6,2] <- se(b0$Pelitigera.neopolydacta)*1.96
st.dev.0[6,3] <- se(b1$Pelitigera.neopolydacta)*1.96

st.dev.0[7,1] <- "Cladonia rangiferina"
st.dev.0[7,2] <- se(b0$Cladonia.rangiferina)*1.96
st.dev.0[7,3] <- se(b1$Cladonia.rangiferina)*1.96

st.dev.0[8,1] <- "Equisetum scripotes"
st.dev.0[8,2] <- se(b0$Equisetum.scripotes)*1.96
st.dev.0[8,3] <- se(b1$Equisetum.scripotes)*1.96

st.dev.0[9,1] <- "Rosa acicularis"
st.dev.0[9,2] <- se(b0$Rosa.acicularis)*1.96
st.dev.0[9,3] <- se(b1$Rosa.acicularis)*1.96

st.dev.0[10,1] <- "Peltigera apthosa"
st.dev.0[10,2] <- se(b0$Peltigera.apthosa)*1.96
st.dev.0[10,3] <- se(b1$Peltigera.apthosa)*1.96

rownames(st.dev.0) <- st.dev.0[,1] ## making species the rownames
st.dev.0 <- as.matrix(st.dev.0[,-1]) ## turning into a matrix without species col
st.dev.0 <- t(st.dev.0) ## transposing the matrix

## 1 fire
st.dev.1 <- data.frame(species = NA, se1 = NA, se2 = NA) ## creating a dataframe to hold SE values, variable naming is innacurate, not calculating SD
st.dev.1[1,1] <- "Ledum decumbens" ## species name
st.dev.1[1,2] <- se(b1$Ledum.decumbens)*1.96
st.dev.1[1,3] <- se(b2$Ledum.decumbens)*1.96

st.dev.1[2,1] <- "Poa arctea"
st.dev.1[2,2] <- se(b1$Poa.arctea)*1.96
st.dev.1[2,3] <- se(b2$Poa.arctea)*1.96

st.dev.1[3,1] <- "Vaccinium viginosum"
st.dev.1[3,2] <- se(b1$Vaccinium.viginosum)*1.96
st.dev.1[3,3] <- se(b2$Vaccinium.viginosum)*1.96

st.dev.1[4,1] <- "Equisetum sylvaticum"
st.dev.1[4,2] <- se(b1$Equisetum.sylvaticum)*1.96
st.dev.1[4,3] <- se(b2$Equisetum.sylvaticum)*1.96

st.dev.1[5,1] <- "Ceratodon purpureus"
st.dev.1[5,2] <- se(b1$Ceratodon.purpureus)*1.96
st.dev.1[5,3] <- se(b2$Ceratodon.purpureus)*1.96

st.dev.1[6,1] <- "Salix spp."
st.dev.1[6,2] <- se(b1$Salix)*1.96
st.dev.1[6,3] <- se(b2$Salix)*1.96

st.dev.1[7,1] <- "Polytrichum strictum"
st.dev.1[7,2] <- se(b1$Polytrichum.strictum)*1.96
st.dev.1[7,3] <- se(b2$Polytrichum.strictum)*1.96

st.dev.1[8,1] <- "Cladonia deformis"
st.dev.1[8,2] <- se(b1$Cladonia.deformis)*1.96
st.dev.1[8,3] <- se(b2$Cladonia.deformis)*1.96

st.dev.1[9,1] <- "Ledum groenlandicum"
st.dev.1[9,2] <- se(b1$Ledum.groenlandicum)*1.96
st.dev.1[9,3] <- se(b2$Ledum.groenlandicum)*1.96

st.dev.1[10,1] <- "Luzula arcuata"
st.dev.1[10,2] <- se(b1$Luzula.arcuata)*1.96
st.dev.1[10,3] <- se(b2$Luzula.arcuata)*1.96

rownames(st.dev.1) <- st.dev.1[,1] ## making species the rownames
st.dev.1 <- as.matrix(st.dev.1[,-1]) ## turning into a matrix without species col
st.dev.1 <- t(st.dev.1) ## transposing the matrix

## 2 fire
st.dev.2 <- data.frame(species = NA, se2 = NA, se3 = NA) ## creating a dataframe to hold SE values, variable naming is innacurate, not calculating SD
st.dev.2[1,1] <- "Equisetum sylvaticum" ## species name
st.dev.2[1,2] <- se(b2$Equisetum.sylvaticum)*1.96
st.dev.2[1,3] <- se(b3$Equisetum.sylvaticum)*1.96

st.dev.2[2,1] <- "Ceratodon purpureus"
st.dev.2[2,2] <- se(b2$Ceratodon.purpureus)*1.96
st.dev.2[2,3] <- se(b3$Ceratodon.purpureus)*1.96

st.dev.2[3,1] <- "Poa pratensis"
st.dev.2[3,2] <- se(b2$Poa.pratensis)*1.96
st.dev.2[3,3] <- se(b3$Poa.pratensis)*1.96

st.dev.2[4,1] <- "Vaccinium viginosum"
st.dev.2[4,2] <- se(b2$Vaccinium.viginosum)*1.96
st.dev.2[4,3] <- se(b3$Vaccinium.viginosum)*1.96

st.dev.2[5,1] <- "Vaccinium vulcanorum"
st.dev.2[5,2] <- se(b2$Vaccinium.vulcanorum)*1.96
st.dev.2[5,3] <- se(b3$Vaccinium.vulcanorum)*1.96

st.dev.2[6,1] <- "Equisetum arvense"
st.dev.2[6,2] <- se(b2$Equisetum.arvense)*1.96
st.dev.2[6,3] <- se(b3$Equisetum.arvense)*1.96

st.dev.2[7,1] <- "Polytrichum commune"
st.dev.2[7,2] <- se(b2$Polytrichum.commune)*1.96
st.dev.2[7,3] <- se(b3$Polytrichum.commune)*1.96

st.dev.2[8,1] <- "Polytrichum strictum"
st.dev.2[8,2] <- se(b2$Polytrichum.strictum)*1.96
st.dev.2[8,3] <- se(b3$Polytrichum.strictum)*1.96

st.dev.2[9,1] <- "Dicranium antifolium"
st.dev.2[9,2] <- se(b2$Dicranium.antifolium)*1.96
st.dev.2[9,3] <- se(b3$Dicranium.antifolium)*1.96

st.dev.2[10,1] <- "Salix spp."
st.dev.2[10,2] <- se(b2$Salix)*1.96
st.dev.2[10,3] <- se(b3$Salix)*1.96

rownames(st.dev.2) <- st.dev.2[,1] ## making species the rownames
st.dev.2 <- as.matrix(st.dev.2[,-1]) ## turning into a matrix without species col
st.dev.2 <- t(st.dev.2) ## transposing the matrix


ze_bar.0 <- barplot(top10.0, 
                    col= pal[1:2] , 
                    border="black", 
                    font.axis= 4,
                    cex.names = 1.75,
                    cex.axis = 1.5,
                    las = 2,
                    ylim = c(0,100),
                    beside=T, 
                    xlab="", 
                    font.lab=2)
mtext(side = 2, line = 3.5, cex = 1.5, "Average Cover (%)")
error.bar(ze_bar.0,top10.0, st.dev.0)
mtext(side = 3, line = .5, cex = 1.5, font = 2, "No Fire - One Fire")
legend(5,100, 
       legend=c("No Fire","1 Fire"), 
       col=pal, 
       ncol = 4,
       pt.cex= 2, 
       cex = 1.5,
       pch=15,
       text.font = 2,
       bty = "n")

ze_bar.1 <- barplot(top10.1, 
                    col= pal[2:3] , 
                    border="black", 
                    font.axis= 4,
                    cex.names = 1.75,
                    cex.axis = 1.5,
                    las = 2,
                    ylim = c(0,100),
                    beside=T, 
                    xlab="", 
                    font.lab=2)
# mtext(side = 2, line = 3, cex = 1.5, "Average Cover (%)")
error.bar(ze_bar.1,top10.1, st.dev.1)
mtext(side = 3, line = .5, cex = 1.5,font = 2,"One Fire - Two Fire")
legend(5,100, 
       legend=c("1 Fire","2 Fires"), 
       col=pal[2:3], 
       ncol = 4,
       pt.cex= 2, 
       cex = 1.5,
       text.font = 2,
       pch=15,
       bty = "n")

ze_bar.2 <- barplot(top10.2, 
                    col= pal[3:4] , 
                    border="black", 
                    font.axis= 4,
                    cex.names = 1.75,
                    cex.axis = 1.5,
                    las = 2,
                    ylim = c(0,100),
                    beside=T, 
                    xlab="", 
                    font.lab=2)
# mtext(side = 2, line = 3, cex = 1.5, "Average Cover (%)")
error.bar(ze_bar.2,top10.2, st.dev.2)
mtext(side = 3, line = .5, cex = 1.5, font = 2, "Two Fire - Three Fire")
legend(5,100, 
       legend=c("2 Fires","3 Fires"), 
       col=pal[3:4], 
       ncol = 4,
       pt.cex=2, 
       text.font = 2,
       cex = 1.5,
       pch=15,
       bty = "n")


#### Summary of Results for Figure 4 ####
length(unique(understory_long$species))  ## 67 unique species
str(understory_long)
Genera <- understory_long %>%
  separate(species, into = c("pre", "post")) %>%
  pull("pre") ## pulling out the genus name for each species
length(unique(Genera)) ## 41 unique genera

table(understory_long$FG)
length(unique(understory_long$species[understory_long$FG == "forb"])) ## 10 forbs
(length(unique(understory_long$species[understory_long$FG == "forb"]))/length(unique(understory_long$species)))*100
## 14.93 % of species are forbs

length(unique(understory_long$species[understory_long$FG == "graminoid"])) ## 5 graminoids
(length(unique(understory_long$species[understory_long$FG == "graminoid"]))/length(unique(understory_long$species)))*100
## 7.46 % of species are graminoids

length(unique(understory_long$species[understory_long$FG == "Lichen"])) ## 14 lichens
(length(unique(understory_long$species[understory_long$FG == "Lichen"]))/length(unique(understory_long$species)))*100
## 20.90 % of species are lichens

length(unique(understory_long$species[understory_long$FG == "Moss"])) ## 20 mosses
(length(unique(understory_long$species[understory_long$FG == "Moss"]))/length(unique(understory_long$species)))*100
## 29.85 % of species are mosses

length(unique(understory_long$species[understory_long$FG == "seedless vascular"])) ## 3 seedless vascular species
(length(unique(understory_long$species[understory_long$FG == "seedless vascular"]))/length(unique(understory_long$species)))*100
## 4.48 % of species are seedless vascular

length(unique(understory_long$species[understory_long$FG == "shrub"])) ## 15 shrubs
(length(unique(understory_long$species[understory_long$FG == "shrub"]))/length(unique(understory_long$species)))*100
## 22.39 % of species are shrubs

## Community Turnover 
## turnover = disappeared + appeared / total number of species (in each community)

understory_turnover <- understory_long[understory_long$plotavg != 0,]

NoFireComm <- unique(understory_turnover$species[understory_turnover$PLOT == 11 | understory_turnover$PLOT == 58 | understory_turnover$PLOT == 10 | understory_turnover$PLOT == 44])
OneFireComm <- unique(understory_turnover$species[understory_turnover$PLOT == 12 | understory_turnover$PLOT == 64 | understory_turnover$PLOT == 65 | understory_turnover$PLOT == 52 | understory_turnover$PLOT == 41 | understory_turnover$PLOT == 48 |understory_turnover$PLOT == 42 | understory_turnover$PLOT == 50])
TwoFireComm <- unique(understory_turnover$species[understory_turnover$PLOT == 32 | understory_turnover$PLOT == 39 | understory_turnover$PLOT == 56 | understory_turnover$PLOT == 57 | understory_turnover$PLOT == 47 | understory_turnover$PLOT == 16 |understory_turnover$PLOT == 8 | understory_turnover$PLOT == 40])
ThreeFireComm <- unique(understory_turnover$species[understory_turnover$PLOT == 37 | understory_turnover$PLOT == 15 | understory_turnover$PLOT == 55 | understory_turnover$PLOT == 54 | understory_turnover$PLOT == 14 | understory_turnover$PLOT == 7])

`%notin%` <- Negate(`%in%`)
length(which(NoFireComm %notin% OneFireComm == TRUE)) ## 7 species disappeared
length(which(OneFireComm %notin% NoFireComm == TRUE)) ## 31 species appeared 
((length(which(NoFireComm %notin% OneFireComm == TRUE))+length(which(OneFireComm %notin% NoFireComm == TRUE)))/(length(NoFireComm)+length(OneFireComm)))*100 
## 61.29 % turnover rate from no fire from one fire

length(which(OneFireComm %notin% TwoFireComm == TRUE)) ## 18 species disappeared
length(which(TwoFireComm %notin% OneFireComm == TRUE)) ## 15 species appeared 
((length(which(OneFireComm %notin% TwoFireComm == TRUE))+length(which(TwoFireComm %notin% OneFireComm == TRUE)))/(length(OneFireComm)+length(TwoFireComm)))*100 
## 39.76 % turnover rate from one fire to two fires

length(which(TwoFireComm %notin% ThreeFireComm == TRUE)) ## 17 species disappeared
length(which(ThreeFireComm %notin% TwoFireComm == TRUE)) ## 8 species appeared 
((length(which(TwoFireComm %notin% ThreeFireComm == TRUE))+length(which(ThreeFireComm %notin% TwoFireComm == TRUE)))/(length(TwoFireComm)+length(ThreeFireComm)))*100 
## 35.21 % turnover rate from two fires to three fires

## adding the full set of pairwise comparisons
length(which(NoFireComm %notin% ThreeFireComm == TRUE)) ## 9 species disappeared
length(which(ThreeFireComm %notin% NoFireComm == TRUE)) ## 21 species appeared 
((length(which(NoFireComm %notin% ThreeFireComm == TRUE))+length(which(ThreeFireComm %notin% NoFireComm == TRUE)))/(length(NoFireComm)+length(ThreeFireComm)))*100 
## 60.00 % turnover rate from no fire to three fires

length(which(NoFireComm %notin% TwoFireComm == TRUE)) ## 9 species disappeared
length(which(TwoFireComm %notin% NoFireComm == TRUE)) ## 30 species appeared 
((length(which(NoFireComm %notin% TwoFireComm == TRUE))+length(which(TwoFireComm %notin% NoFireComm == TRUE)))/(length(NoFireComm)+length(TwoFireComm)))*100 
## 66.10 % turnover rate from no fire to two fires

length(which(OneFireComm %notin% ThreeFireComm == TRUE)) ## 19 species disappeared
length(which(ThreeFireComm %notin% OneFireComm == TRUE)) ## 7 species appeared 
((length(which(OneFireComm %notin% ThreeFireComm == TRUE))+length(which(ThreeFireComm %notin% OneFireComm == TRUE)))/(length(OneFireComm)+length(ThreeFireComm)))*100 
## 35.14 % turnover rate from one fire to three fires


rm(b0);rm(b1);rm(b2);rm(b3);rm(burn.0);rm(burn.1);rm(burn.2);rm(burn.sum)
rm(st.dev.0);rm(st.dev.1);rm(st.dev.2);rm(top10.0);rm(top10.1);rm(top10.2)
rm(error.bar);rm(se);rm(`%notin%`);rm(understory_turnover);rm(understory_long_FG);rm(understory_wide_FG)
rm(NoFireComm);rm(OneFireComm);rm(TwoFireComm);rm(ThreeFireComm);rm(understory_FireMat)
rm(ze_bar);rm(ze_bar.0);rm(ze_bar.1);rm(ze_bar.2);rm(pal);rm(pal2);rm(Genera)


#### Table 1 - GLM results ####
env <- read.csv("site_attributes.csv")
soil <- read.csv("org_depth.csv")
env <- env[env$SITE != "STEESE",]
soil <- soil[soil$SITE != "STEESE",]

soil <- soil %>% 
  group_by(PLOT) %>% 
  summarise(ORG_DEPTH_cm = mean(ORG_DEPTH_cm)) 
soil <- as.data.frame(soil) ## converting the tibble to a data frame

soil$PLOT[soil$PLOT == "7_3"] <- "07_3"
soil$PLOT[soil$PLOT == "8_2"] <- "08_2"
env$SITECODE[env$SITECODE == "7_3"] <- "07_3"
env$SITECODE[env$SITECODE == "8_2"] <- "08_2"
env <- env[order(env$SITECODE),]
soil <- soil[order(soil$PLOT),]

understory <- cbind(understory, env)
understory <- cbind(understory, soil)

understory <- understory[,c(2:4,10,13,15)]

understory.burned <- understory[understory$treatment != 0,]


par(mfrow = c(1,1))
## writing a series of models and then have to make sure the effect sizes are standardized
## three for richness
m1 <- glm(Richness ~ SLOPE,
          data = understory.burned,
          family = poisson(link = "log"))
summary(m1)
pseudo.R.squared(m1)
plot(residuals(m1)); abline(h = 0, col = "red", lwd = 2, lty = 2)
qqnorm(residuals(m1), pch = 16, cex = .75, col = rgb(0,0,0,0.75))
qqline(residuals(m1), col = "red", lwd = 2, lty = 2)

m2 <- glm(Richness ~ SOLAR,
          data = understory.burned,
          family = poisson(link = "log"))
summary(m2)
pseudo.R.squared(m2) 
plot(residuals(m2)); abline(h = 0, col = "red", lwd = 2, lty = 2)
qqnorm(residuals(m2), pch = 16, cex = .75, col = rgb(0,0,0,0.75))
qqline(residuals(m2), col = "red", lwd = 2, lty = 2)

m3 <- glm(Richness ~ ORG_DEPTH_cm,
          data = understory.burned,
          family = poisson(link = "log"))
summary(m3)
pseudo.R.squared(m3)
plot(residuals(m3)); abline(h = 0, col = "red", lwd = 2, lty = 2)
qqnorm(residuals(m3), pch = 16, cex = .75, col = rgb(0,0,0,0.75))
qqline(residuals(m3), col = "red", lwd = 2, lty = 2)

## three for diversity
m4 <- glm(simpson ~ SLOPE,
          data = understory.burned)
summary(m4)
pseudo.R.squared(m4)
plot(residuals(m4)); abline(h = 0, col = "red", lwd = 2, lty = 2)
qqnorm(residuals(m4), pch = 16, cex = .75, col = rgb(0,0,0,0.75))
qqline(residuals(m4), col = "red", lwd = 2, lty = 2)

m5 <- glm(simpson ~ SOLAR,
          data = understory.burned)
summary(m5)
pseudo.R.squared(m5)
plot(residuals(m5)); abline(h = 0, col = "red", lwd = 2, lty = 2)
qqnorm(residuals(m5), pch = 16, cex = .75, col = rgb(0,0,0,0.75))
qqline(residuals(m5), col = "red", lwd = 2, lty = 2)

m6 <- glm(simpson ~ ORG_DEPTH_cm,
          data = understory.burned)
summary(m6)
pseudo.R.squared(m6)
plot(residuals(m6)); abline(h = 0, col = "red", lwd = 2, lty = 2)
qqnorm(residuals(m6), pch = 16, cex = .75, col = rgb(0,0,0,0.75))
qqline(residuals(m6), col = "red", lwd = 2, lty = 2)

pwr.f2.test(u = 20,v = 21, f2 = NULL, sig.level = 0.05, power = 0.95)

## getting standardized effect sizes from the predict function
newdata.SLOPE <-data.frame(SLOPE = seq(min(understory.burned$SLOPE), max(understory.burned$SLOPE), length.out = 30))
newdata.SOLAR <-data.frame(SOLAR = seq(min(understory.burned$SOLAR), max(understory.burned$SOLAR), length.out = 30))
newdata.ORG_DEPTH <-data.frame(ORG_DEPTH_cm = seq(min(understory.burned$ORG_DEPTH_cm), max(understory.burned$ORG_DEPTH_cm), length.out = 30))

preds1 <- predict(m1, newdata.SLOPE, type="response", se.fit = TRUE)
(preds1$fit[30] - preds1$fit[1])/(newdata.SLOPE$SLOPE[30] - newdata.SLOPE$SLOPE[1]) ## average number of species increase per 1 degree of slope
((preds1$fit[30] - 1.96*preds1$se.fit[30]) - (preds1$fit[1] - 1.96*preds1$se.fit[1]))/(newdata.SLOPE$SLOPE[30] - newdata.SLOPE$SLOPE[1]) ## lower bound of 95% CI increase in number of species per 1 degree of slope
((preds1$fit[30] + 1.96*preds1$se.fit[30]) - (preds1$fit[1] + 1.96*preds1$se.fit[1]))/(newdata.SLOPE$SLOPE[30] - newdata.SLOPE$SLOPE[1]) ## upper bound of 95% CI increase in number of species per 1 degree of slope

preds2 <- predict(m2, newdata.SOLAR, type="response", se.fit = TRUE)
(preds2$fit[30] - preds2$fit[1])/(newdata.SOLAR$SOLAR[30] - newdata.SOLAR$SOLAR[1]) ## average number of species increase per 1 degree of slope
((preds2$fit[30] - 1.96*preds2$se.fit[30]) - (preds2$fit[1] - 1.96*preds2$se.fit[1]))/(newdata.SOLAR$SOLAR[30] - newdata.SOLAR$SOLAR[1]) ## lower bound of 95% CI increase in number of species per 1 degree of slope
((preds2$fit[30] + 1.96*preds2$se.fit[30]) - (preds2$fit[1] + 1.96*preds2$se.fit[1]))/(newdata.SOLAR$SOLAR[30] - newdata.SOLAR$SOLAR[1]) ## upper bound of 95% CI increase in number of species per 1 degree of slope

preds3 <- predict(m3, newdata.ORG_DEPTH, type="response", se.fit = TRUE)
(preds3$fit[30] - preds3$fit[1])/(newdata.ORG_DEPTH$ORG_DEPTH_cm[30] - newdata.ORG_DEPTH$ORG_DEPTH_cm[1]) ## average number of species increase per 1 degree of slope
((preds3$fit[30] - 1.96*preds3$se.fit[30]) - (preds3$fit[1] - 1.96*preds3$se.fit[1]))/(newdata.ORG_DEPTH$ORG_DEPTH_cm[30] - newdata.ORG_DEPTH$ORG_DEPTH_cm[1]) ## lower bound of 95% CI increase in number of species per 1 degree of slope
((preds3$fit[30] + 1.96*preds3$se.fit[30]) - (preds3$fit[1] + 1.96*preds3$se.fit[1]))/(newdata.ORG_DEPTH$ORG_DEPTH_cm[30] - newdata.ORG_DEPTH$ORG_DEPTH_cm[1]) ## upper bound of 95% CI increase in number of species per 1 degree of slope

preds4 <- predict(m4, newdata.SLOPE, type="response", se.fit = TRUE)
(preds4$fit[30] - preds4$fit[1])/(newdata.SLOPE$SLOPE[30] - newdata.SLOPE$SLOPE[1]) ## average diversity increase per 1 degree of slope
((preds4$fit[30] - 1.96*preds4$se.fit[30]) - (preds4$fit[1] - 1.96*preds4$se.fit[1]))/(newdata.SLOPE$SLOPE[30] - newdata.SLOPE$SLOPE[1]) ## lower bound of 95% CI increase in diversity per 1 degree of slope
((preds4$fit[30] + 1.96*preds4$se.fit[30]) - (preds4$fit[1] + 1.96*preds4$se.fit[1]))/(newdata.SLOPE$SLOPE[30] - newdata.SLOPE$SLOPE[1]) ## upper bound of 95% CI increase in diversity per 1 degree of slope

preds5 <- predict(m5, newdata.SOLAR, type="response", se.fit = TRUE)
(preds5$fit[30] - preds5$fit[1])/(newdata.SOLAR$SOLAR[30] - newdata.SOLAR$SOLAR[1]) ## average diversity increase per 1 degree of slope
((preds5$fit[30] - 1.96*preds5$se.fit[30]) - (preds5$fit[1] - 1.96*preds5$se.fit[1]))/(newdata.SOLAR$SOLAR[30] - newdata.SOLAR$SOLAR[1]) ## lower bound of 95% CI increase in diversity per 1 degree of slope
((preds5$fit[30] + 1.96*preds5$se.fit[30]) - (preds5$fit[1] + 1.96*preds5$se.fit[1]))/(newdata.SOLAR$SOLAR[30] - newdata.SOLAR$SOLAR[1]) ## upper bound of 95% CI increase in diversity per 1 degree of slope

preds6 <- predict(m6, newdata.ORG_DEPTH, type="response", se.fit = TRUE)
(preds6$fit[30] - preds6$fit[1])/(newdata.ORG_DEPTH$ORG_DEPTH_cm[30] - newdata.ORG_DEPTH$ORG_DEPTH_cm[1]) ## average diversity increase per 1 degree of slope
((preds6$fit[30] - 1.96*preds6$se.fit[30]) - (preds6$fit[1] - 1.96*preds6$se.fit[1]))/(newdata.ORG_DEPTH$ORG_DEPTH_cm[30] - newdata.ORG_DEPTH$ORG_DEPTH_cm[1]) ## lower bound of 95% CI increase in diversity per 1 degree of slope
((preds6$fit[30] + 1.96*preds6$se.fit[30]) - (preds6$fit[1] + 1.96*preds6$se.fit[1]))/(newdata.ORG_DEPTH$ORG_DEPTH_cm[30] - newdata.ORG_DEPTH$ORG_DEPTH_cm[1]) ## upper bound of 95% CI increase in diversity per 1 degree of slope


#### Table 1 - GLM results (global model) ####
## testing if we include a partial effects model for each what might happen
## using these as it is an easier explanation
m7 <- glm(Richness ~ SOLAR + ORG_DEPTH_cm,
          data = understory.burned,
          family = poisson(link = "log"))
summary(m7)
pseudo.R.squared(m7)
plot(residuals(m7)); abline(h = 0, col = "red", lwd = 2, lty = 2)
qqnorm(residuals(m7), pch = 16, cex = .75, col = rgb(0,0,0,0.75))
qqline(residuals(m7), col = "red", lwd = 2, lty = 2)

m8 <- glm(simpson ~ SOLAR + ORG_DEPTH_cm,
          data = understory.burned)
summary(m8)
pseudo.R.squared(m8)
plot(residuals(m8)); abline(h = 0, col = "red", lwd = 2, lty = 2)
qqnorm(residuals(m8), pch = 16, cex = .75, col = rgb(0,0,0,0.75))
qqline(residuals(m8), col = "red", lwd = 2, lty = 2)


pwr.f2.test(u = 19,v = 21, f2 = NULL, sig.level = 0.05, power = 0.95)

## determining which species are present in which fire categories for supplement
SpPl <- data.frame(Sp = unique(understory_long$species), Pl = NA)
Sp <- unique(understory_long$species)
for(i in 1:length(Sp)){
  tmp <- understory_long[understory_long$species == Sp[i],]
  tmp <- tmp[tmp$plotavg != 0,]
  SpPl[i,2] <- paste(unique(understory_plots$treat[understory_plots$plot %in% tmp$PLOT]), collapse = " ")
  # SpPl[i,2] <- paste(unique(understory_plots$treat[tmp$PLOT %in% understory_plots$plot]), collapse = " ")
  
}
SpPl$FG <- understory_long$FG[match(SpPl$Sp,understory_long$species)]
getwd()
# write.csv(SpPl, "SpeciesFunctionalGroupsBurns.csv")
