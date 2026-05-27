## --------------- HEADER ------------------------------------------------------
## Script name: 4c_Disturbance-veg-survey-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-08-05
## Date Last Modified: 2023-08-05
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script analyzes the soil disturbance
## plant vegetation survey

# Clear the decks
rm(list=ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

env <- surv[,1:7]
spec <- surv[,8:80]
spec$BLANK <- 1

betad <- betadiver(spec, "z")

mod <- with(env, betadisper(betad, TREATMENT))
plot(mod)
boxplot(mod)
anova(mod)

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

REF <- surv |> filter(TREATMENT == 'REF')
CC <- surv |> filter(TREATMENT == 'CC')
OC <- surv |> filter(TREATMENT == 'OC')

REF.spec <- REF[,8:105]
REF.env <- REF[,1:7]

CC.spec <- CC[,8:105]
CC.env <- CC[,1:7]

OC.spec <- OC[,8:105]
OC.env <- OC[,1:7]

# Ref 
betad <- betadiver(REF.spec, "z")

mod <- with(REF.env, betadisper(betad, BLOCK))
plot(mod)
boxplot(mod)
anova(mod)

# CC 
betad <- betadiver(CC.spec, "z")

mod <- with(CC.env, betadisper(betad, BLOCK))
plot(mod)
boxplot(mod)
anova(mod)

# OC 
betad <- betadiver(OC.spec, "z")

mod <- with(OC.env, betadisper(betad, BLOCK))
plot(mod)
boxplot(mod)
anova(mod)

# Ordinations

# Ref
REF.spec <- REF.spec[,colSums(REF.spec[8:98]) > 0]
REF.spec$BLANK <- 1
spec.dist <- vegdist(REF.spec)
nMDS <- metaMDS(REF.spec, trymax = 100)

REF.env$BLOCK.TREAT <- paste(REF.env$BLOCK, REF.env$TREATMENT)

plot(nMDS, display = 'sites')
with(REF.env, ordihull(nMDS, BLOCK, col="blue", lty=2, label = TRUE))
with(REF.env, ordispider(nMDS, BLOCK.TREAT, col = "black", label= TRUE))

# CC
CC.spec <- CC.spec[,colSums(CC.spec[8:98]) > 0]
CC.spec$BLANK <- 1
spec.dist <- vegdist(CC.spec)
nMDS <- metaMDS(CC.spec, trymax = 100)

CC.env$BLOCK.TREAT <- paste(CC.env$BLOCK, CC.env$TREATMENT)

plot(nMDS, display = 'sites')
with(CC.env, ordihull(nMDS, BLOCK, col="blue", lty=2, label = TRUE))
with(CC.env, ordispider(nMDS, BLOCK.TREAT, col = "black", label= TRUE))

# OC
OC.spec <- OC.spec[,colSums(OC.spec[8:98]) > 0]
OC.spec$BLANK <- 1
spec.dist <- vegdist(OC.spec)
nMDS <- metaMDS(OC.spec, trymax = 100)

OC.env$BLOCK.TREAT <- paste(OC.env$BLOCK, OC.env$TREATMENT)

plot(nMDS, display = 'sites')
with(OC.env, ordihull(nMDS, BLOCK, col="blue", lty=2, label = TRUE))
with(OC.env, ordispider(nMDS, BLOCK.TREAT, col = "black", label= TRUE))

# Check out vegetation communities at 2-3 m by block

# Clear the decks
rm(list=ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

b4 <- surv |> filter(BLOCK == '4')
b5 <- surv |> filter(BLOCK == '5')
b11 <- surv |> filter(BLOCK == '11')
b19 <- surv |> filter(BLOCK == '19')
b20 <- surv |> filter(BLOCK == '20')
b18 <- surv |> filter(BLOCK == '18')
b37 <- surv |> filter(BLOCK == '37')
b38 <- surv |> filter(BLOCK == '38')

b4.spec <- b4[,8:105]
b4.env <- b4[,1:7]
b4.spec <- b4.spec[,colSums(b4.spec[8:98]) > 0]

b5.spec <- b5[,8:105]
b5.env <- b5[,1:7]
b5.spec <- b5.spec[,colSums(b5.spec[8:98]) > 0]

b11.spec <- b11[,8:105]
b11.env <- b11[,1:7]
b11.spec <- b11.spec[,colSums(b11.spec[8:98]) > 0]

b19.spec <- b19[,8:105]
b19.env <- b19[,1:7]
b19.spec <- b19.spec[,colSums(b19.spec[8:98]) > 0]

b20.spec <- b20[,8:105]
b20.env <- b20[,1:7]
b20.spec <- b20.spec[,colSums(b20.spec[8:98]) > 0]

b18.spec <- b18[,8:105]
b18.env <- b18[,1:7]
b18.spec <- b18.spec[,colSums(b18.spec[8:98]) > 0]

b37.spec <- b37[,8:105]
b37.env <- b37[,1:7]
b37.spec <- b37.spec[,colSums(b37.spec[8:98]) > 0]

b38.spec <- b38[,8:105]
b38.env <- b38[,1:7]
b38.spec <- b38.spec[,colSums(b38.spec[8:98]) > 0]

# Calculate centroid distances for vegetation 2-3 m from plot center

# B 4
b4.spec$BLANK <- 1
spec.dist <- vegdist(b4.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b4.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

library(usedist)
b4.CC.dist <- dist_between_centroids(spec.dist, 1:8, 17:24 )
b4.OC.dist <- dist_between_centroids(spec.dist, 9:16, 17:24 )

# B 5
b5.spec$BLANK <- 1
spec.dist <- vegdist(b5.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b5.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b5.CC.dist <- dist_between_centroids(spec.dist, 9:16, 17:24 )
b5.OC.dist <- dist_between_centroids(spec.dist, 1:8, 17:24 )

# B 11
b11.spec$BLANK <- 1
spec.dist <- vegdist(b11.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b11.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b11.CC.dist <- dist_between_centroids(spec.dist, 9:16, 17:24 )
b11.OC.dist <- dist_between_centroids(spec.dist, 1:8, 17:24 )

# B 18
b18.spec$BLANK <- 1
spec.dist <- vegdist(b18.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b18.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b18.CC.dist <- dist_between_centroids(spec.dist, 9:16, 17:24 )
b18.OC.dist <- dist_between_centroids(spec.dist, 1:8, 17:24 )

# B 19
b19.spec$BLANK <- 1
spec.dist <- vegdist(b19.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b19.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b19.CC.dist <- dist_between_centroids(spec.dist, 1:8, 17:24 )
b19.OC.dist <- dist_between_centroids(spec.dist, 9:16, 17:24 )

# B 20
b20.spec$BLANK <- 1
spec.dist <- vegdist(b20.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b20.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b20.CC.dist <- dist_between_centroids(spec.dist, 1:8, 17:24 )
b20.OC.dist <- dist_between_centroids(spec.dist, 9:16, 17:24 )

# B 37
b37.spec$BLANK <- 1
spec.dist <- vegdist(b37.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b37.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b37.CC.dist <- dist_between_centroids(spec.dist, 1:8, 17:24 )
b37.OC.dist <- dist_between_centroids(spec.dist, 9:16, 17:24 )

# B 38
b38.spec$BLANK <- 1
spec.dist <- vegdist(b38.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b38.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b38.CC.dist <- dist_between_centroids(spec.dist, 1:8, 17:24 )
b38.OC.dist <- dist_between_centroids(spec.dist, 9:16, 17:24 )

# Some notes
# Distances
# 4 OC farther
	# dist 1.5, 1, 0.5, 0.5
mean(1.5, 1, 0.5, 0.5) # mean 1.5
# 1 overlap
# 3 CC is closer
	# dist 0.5, 0.75, 1
mean(0.5,0.75,1) # mean 0.5

# Dispersion
# 5 OC
# 2 similar
# 1 CC 
# Ref is smaller in 3

# Create dataframe
far.centroid.dist <- tibble(
	BLOCK = c(4, 4, 5, 5, 11, 11, 18, 18, 19, 19, 20, 20, 37, 37, 38, 38),
	TREATMENT = c('CC', 'OC', 'CC', 'OC', 'CC', 'OC', 'CC', 'OC',
								'CC', 'OC', 'CC', 'OC', 'CC', 'OC', 'CC', 'OC'),
	DISTANCE = c(b4.CC.dist, b4.OC.dist, b5.CC.dist, b5.OC.dist,
							 b11.CC.dist, b11.OC.dist, b18.CC.dist, b18.OC.dist,
							 b19.CC.dist, b19.OC.dist, b20.CC.dist, b20.OC.dist,
							 b37.CC.dist, b37.OC.dist, b38.CC.dist, b38.OC.dist))

far.centroid.dist |>
	group_by(TREATMENT) |>
	summarize(MEAN.DIST = mean(DISTANCE))

# Check out vegetation communities at 0-1 m by block

# Clear the decks
rm(list=ls()[! ls() %in% c("far.centroid.dist")])

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 0 | METER == 1) |>
	na.omit()

b4 <- surv |> filter(BLOCK == '4')
b5 <- surv |> filter(BLOCK == '5')
b11 <- surv |> filter(BLOCK == '11')
b19 <- surv |> filter(BLOCK == '19')
b20 <- surv |> filter(BLOCK == '20')
b18 <- surv |> filter(BLOCK == '18')
b37 <- surv |> filter(BLOCK == '37')
b38 <- surv |> filter(BLOCK == '38')

b4.spec <- b4[,8:105]
b4.env <- b4[,1:7]
b4.spec <- b4.spec[,colSums(b4.spec[8:98]) > 0]

b5.spec <- b5[,8:105]
b5.env <- b5[,1:7]
b5.spec <- b5.spec[,colSums(b5.spec[8:98]) > 0]

b11.spec <- b11[,8:105]
b11.env <- b11[,1:7]
b11.spec <- b11.spec[,colSums(b11.spec[8:98]) > 0]

b19.spec <- b19[,8:105]
b19.env <- b19[,1:7]
b19.spec <- b19.spec[,colSums(b19.spec[8:98]) > 0]

b20.spec <- b20[,8:105]
b20.env <- b20[,1:7]
b20.spec <- b20.spec[,colSums(b20.spec[8:98]) > 0]

b18.spec <- b18[,8:105]
b18.env <- b18[,1:7]
b18.spec <- b18.spec[,colSums(b18.spec[8:98]) > 0]

b37.spec <- b37[,8:105]
b37.env <- b37[,1:7]
b37.spec <- b37.spec[,colSums(b37.spec[8:98]) > 0]

b38.spec <- b38[,8:105]
b38.env <- b38[,1:7]
b38.spec <- b38.spec[,colSums(b38.spec[8:98]) > 0]

# Calculate centroid distances for vegetation 0-1 m from plot center

# B 4
b4.spec$BLANK <- 1
spec.dist <- vegdist(b4.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b4.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

library(usedist)
b4.CC.dist <- dist_between_centroids(spec.dist, 1:5, 11:15)
b4.OC.dist <- dist_between_centroids(spec.dist, 6:10, 11:15)

# B 5
b5.spec$BLANK <- 1
spec.dist <- vegdist(b5.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b5.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b5.CC.dist <- dist_between_centroids(spec.dist, 1:5, 11:15)
b5.OC.dist <- dist_between_centroids(spec.dist, 6:10, 11:15)

# B 11
b11.spec$BLANK <- 1
spec.dist <- vegdist(b11.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b11.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b11.CC.dist <- dist_between_centroids(spec.dist, 6:10, 11:15)
b11.OC.dist <- dist_between_centroids(spec.dist, 1:5, 11:15)

# B 18
b18.spec$BLANK <- 1
spec.dist <- vegdist(b18.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b18.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b18.CC.dist <- dist_between_centroids(spec.dist, 6:10, 11:15)
b18.OC.dist <- dist_between_centroids(spec.dist, 1:5, 11:15)

# B 19
b19.spec$BLANK <- 1
spec.dist <- vegdist(b19.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b19.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b19.CC.dist <- dist_between_centroids(spec.dist, 1:5, 11:15)
b19.OC.dist <- dist_between_centroids(spec.dist, 6:10, 11:15)

# B 20
b20.spec$BLANK <- 1
spec.dist <- vegdist(b20.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b20.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b20.CC.dist <- dist_between_centroids(spec.dist, 1:5, 11:15)
b20.OC.dist <- dist_between_centroids(spec.dist, 6:10, 11:15)

# B 37
b37.spec$BLANK <- 1
spec.dist <- vegdist(b37.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b37.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b37.CC.dist <- dist_between_centroids(spec.dist, 1:5, 11:15)
b37.OC.dist <- dist_between_centroids(spec.dist, 6:10, 11:15)

# B 38
b38.spec$BLANK <- 1
spec.dist <- vegdist(b38.spec)
nMDS <- metaMDS(spec.dist, trymax = 100)
plot(nMDS, display = 'sites')
with(b38.env, ordispider(nMDS, TREATMENT, col = "black", label= TRUE))

b38.CC.dist <- dist_between_centroids(spec.dist, 1:5, 11:15)
b38.OC.dist <- dist_between_centroids(spec.dist, 6:10, 11:15)

# Create dataframe
close.centroid.dist <- tibble(
	BLOCK = c(4, 4, 5, 5, 11, 11, 18, 18, 19, 19, 20, 20, 37, 37, 38, 38),
	TREATMENT = c('CC', 'OC', 'CC', 'OC', 'CC', 'OC', 'CC', 'OC',
								'CC', 'OC', 'CC', 'OC', 'CC', 'OC', 'CC', 'OC'),
	DISTANCE = c(b4.CC.dist, b4.OC.dist, b5.CC.dist, b5.OC.dist,
							 b11.CC.dist, b11.OC.dist, b18.CC.dist, b18.OC.dist,
							 b19.CC.dist, b19.OC.dist, b20.CC.dist, b20.OC.dist,
							 b37.CC.dist, b37.OC.dist, b38.CC.dist, b38.OC.dist))

close.centroid.dist |>
	group_by(TREATMENT) |>
	summarize(MEAN.DIST = mean(DISTANCE))

close.centroid.dist$METER <- 'CLOSE'
far.centroid.dist$METER <- 'FAR'

centroid.dist <- rbind(close.centroid.dist, far.centroid.dist)
rm(far.centroid.dist, close.centroid.dist)

# Dispersion 

# Clear the decks
rm(list=ls()[! ls() %in% c("centroid.dist")])

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

# FAR

b4 <- surv |> filter(BLOCK == '4')
b5 <- surv |> filter(BLOCK == '5')
b11 <- surv |> filter(BLOCK == '11')
b19 <- surv |> filter(BLOCK == '19')
b20 <- surv |> filter(BLOCK == '20')
b18 <- surv |> filter(BLOCK == '18')
b37 <- surv |> filter(BLOCK == '37')
b38 <- surv |> filter(BLOCK == '38')

b4.spec <- b4[,8:105]
b4.env <- b4[,1:7]
b4.spec <- b4.spec[,colSums(b4.spec[8:98]) > 0]
b4.spec$BLANK <- 1

b5.spec <- b5[,8:105]
b5.env <- b5[,1:7]
b5.spec <- b5.spec[,colSums(b5.spec[8:98]) > 0]
b5.spec$BLANK <- 1

b11.spec <- b11[,8:105]
b11.env <- b11[,1:7]
b11.spec <- b11.spec[,colSums(b11.spec[8:98]) > 0]
b11.spec$BLANK <- 1

b19.spec <- b19[,8:105]
b19.env <- b19[,1:7]
b19.spec <- b19.spec[,colSums(b19.spec[8:98]) > 0]
b19.spec$BLANK <- 1

b20.spec <- b20[,8:105]
b20.env <- b20[,1:7]
b20.spec <- b20.spec[,colSums(b20.spec[8:98]) > 0]
b20.spec$BLANK <- 1

b18.spec <- b18[,8:105]
b18.env <- b18[,1:7]
b18.spec <- b18.spec[,colSums(b18.spec[8:98]) > 0]
b18.spec$BLANK <- 1

b37.spec <- b37[,8:105]
b37.env <- b37[,1:7]
b37.spec <- b37.spec[,colSums(b37.spec[8:98]) > 0]
b37.spec$BLANK <- 1

b38.spec <- b38[,8:105]
b38.env <- b38[,1:7]
b38.spec <- b38.spec[,colSums(b38.spec[8:98]) > 0]
b38.spec$BLANK <- 1

betad <- betadiver(b4.spec, "z")
mod <- with(b4.env, betadisper(betad, TREATMENT))
boxplot(mod)
b4.CC.dist <- 0.4141
b4.OC.dist <- 0.3980
b4.REF.dist <- 0.2056

betad <- betadiver(b5.spec, "z")
mod <- with(b5.env, betadisper(betad, TREATMENT))
boxplot(mod)
b5.CC.dist <- 0.3502
b5.OC.dist <- 0.3571
b5.REF.dist <- 0.2768

betad <- betadiver(b11.spec, "z")
mod <- with(b11.env, betadisper(betad, TREATMENT))
boxplot(mod)
b11.CC.dist <- 0.2762
b11.OC.dist <- 0.3087
b11.REF.dist <- 0.2637

betad <- betadiver(b18.spec, "z")
mod <- with(b18.env, betadisper(betad, TREATMENT))
b18.CC.dist <- 0.1176
b18.OC.dist <- 0.2918
b18.REF.dist <- 0.1908

betad <- betadiver(b19.spec, "z")
mod <- with(b19.env, betadisper(betad, TREATMENT))
boxplot(mod)
b19.CC.dist <- 0.06576
b19.OC.dist <- 0.08476
b19.REF.dist <- 0.17688

betad <- betadiver(b20.spec, "z")
mod <- with(b20.env, betadisper(betad, TREATMENT))
boxplot(mod)
boxplot(mod)
b20.CC.dist <- 0.2994
b20.OC.dist <- 0.2552
b20.REF.dist <- 0.3773

betad <- betadiver(b37.spec, "z")
mod <- with(b37.env, betadisper(betad, TREATMENT))
b37.CC.dist <- 0.2310
b37.OC.dist <- 0.2178
b37.REF.dist <- 0.3471

betad <- betadiver(b38.spec, "z")
mod <- with(b38.env, betadisper(betad, TREATMENT))
b38.CC.dist <- 0.2356
b38.OC.dist <- 0.1716
b38.REF.dist <- 0.1695

# Create dataframe
far.dispersion <- tibble(
	BLOCK = c(4, 4, 4, 5, 5, 5, 11, 11, 11, 18, 18, 18, 19, 19, 19, 
						20, 20, 20, 37, 37, 37, 38, 38, 38),
	TREATMENT = c('CC', 'OC', 'REF', 'CC', 'OC', 'REF', 'CC', 'OC', 'REF',
								'CC', 'OC', 'REF', 'CC', 'OC', 'REF', 'CC', 'OC', 'REF',
								'CC', 'OC', 'REF', 'CC', 'OC',  'REF'),
	DISTANCE = c(b4.CC.dist, b4.OC.dist, b4.REF.dist, b5.CC.dist, b5.OC.dist,
							 b5.REF.dist, b11.CC.dist, b11.OC.dist, b11.REF.dist, b18.CC.dist, 
							 b18.OC.dist, b18.REF.dist, b19.CC.dist, b19.OC.dist, b19.REF.dist,
							 b20.CC.dist, b20.OC.dist, b20.REF.dist, b37.CC.dist, b37.OC.dist, 
							 b37.REF.dist, b38.CC.dist, b38.OC.dist, b38.REF.dist))

far.dispersion |>
	group_by(TREATMENT) |>
	summarize(Dispersion = mean(DISTANCE))

far.dispersion$METER <- 'Far'

# Clear the decks
rm(list=ls()[! ls() %in% c("centroid.dist", 'far.dispersion')])

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 0 | METER == 1) |>
	na.omit()

# FAR

b4 <- surv |> filter(BLOCK == '4')
b5 <- surv |> filter(BLOCK == '5')
b11 <- surv |> filter(BLOCK == '11')
b19 <- surv |> filter(BLOCK == '19')
b20 <- surv |> filter(BLOCK == '20')
b18 <- surv |> filter(BLOCK == '18')
b37 <- surv |> filter(BLOCK == '37')
b38 <- surv |> filter(BLOCK == '38')

b4.spec <- b4[,8:105]
b4.env <- b4[,1:7]
b4.spec <- b4.spec[,colSums(b4.spec[8:98]) > 0]
b4.spec$BLANK <- 1

b5.spec <- b5[,8:105]
b5.env <- b5[,1:7]
b5.spec <- b5.spec[,colSums(b5.spec[8:98]) > 0]
b5.spec$BLANK <- 1

b11.spec <- b11[,8:105]
b11.env <- b11[,1:7]
b11.spec <- b11.spec[,colSums(b11.spec[8:98]) > 0]
b11.spec$BLANK <- 1

b19.spec <- b19[,8:105]
b19.env <- b19[,1:7]
b19.spec <- b19.spec[,colSums(b19.spec[8:98]) > 0]
b19.spec$BLANK <- 1

b20.spec <- b20[,8:105]
b20.env <- b20[,1:7]
b20.spec <- b20.spec[,colSums(b20.spec[8:98]) > 0]
b20.spec$BLANK <- 1

b18.spec <- b18[,8:105]
b18.env <- b18[,1:7]
b18.spec <- b18.spec[,colSums(b18.spec[8:98]) > 0]
b18.spec$BLANK <- 1

b37.spec <- b37[,8:105]
b37.env <- b37[,1:7]
b37.spec <- b37.spec[,colSums(b37.spec[8:98]) > 0]
b37.spec$BLANK <- 1

b38.spec <- b38[,8:105]
b38.env <- b38[,1:7]
b38.spec <- b38.spec[,colSums(b38.spec[8:98]) > 0]
b38.spec$BLANK <- 1

betad <- betadiver(b4.spec, "z")
mod <- with(b4.env, betadisper(betad, TREATMENT))
boxplot(mod)
b4.CC.dist <- 0.4182
b4.OC.dist <- 0.2735
b4.REF.dist <- 0.1240

betad <- betadiver(b5.spec, "z")
mod <- with(b5.env, betadisper(betad, TREATMENT))
boxplot(mod)
b5.CC.dist <- 0.1052
b5.OC.dist <- 0.2438
b5.REF.dist <- 0.2736

betad <- betadiver(b11.spec, "z")
options(scipen=999) 
mod <- with(b11.env, betadisper(betad, TREATMENT))
boxplot(mod)
b11.CC.dist <- 0.1052
b11.OC.dist <- 0.0000
b11.REF.dist <- 0.1052

betad <- betadiver(b18.spec, "z")
mod <- with(b18.env, betadisper(betad, TREATMENT))
b18.CC.dist <- 0.23379
b18.OC.dist <- 0.29840
b18.REF.dist <- 0.05261

betad <- betadiver(b19.spec, "z")
mod <- with(b19.env, betadisper(betad, TREATMENT))
boxplot(mod)
b19.CC.dist <- 0.2431
b19.OC.dist <- 0.2821
b19.REF.dist <- 0.1643

betad <- betadiver(b20.spec, "z")
mod <- with(b20.env, betadisper(betad, TREATMENT))
boxplot(mod)
b20.CC.dist <- 0.3470
b20.OC.dist <- 0.3693
b20.REF.dist <- 0.2202

betad <- betadiver(b37.spec, "z")
mod <- with(b37.env, betadisper(betad, TREATMENT))
b37.CC.dist <- 0.2633
b37.OC.dist <- 0.2262
b37.REF.dist <- 0.3061

betad <- betadiver(b38.spec, "z")
mod <- with(b38.env, betadisper(betad, TREATMENT))
b38.CC.dist <- 0.08301
b38.OC.dist <- 0.10521
b38.REF.dist <- 0.16601

# Create dataframe
close.dispersion <- tibble(
	BLOCK = c(4, 4, 4, 5, 5, 5, 11, 11, 11, 18, 18, 18, 19, 19, 19, 
						20, 20, 20, 37, 37, 37, 38, 38, 38),
	TREATMENT = c('CC', 'OC', 'REF', 'CC', 'OC', 'REF', 'CC', 'OC', 'REF',
								'CC', 'OC', 'REF', 'CC', 'OC', 'REF', 'CC', 'OC', 'REF',
								'CC', 'OC', 'REF', 'CC', 'OC',  'REF'),
	DISTANCE = c(b4.CC.dist, b4.OC.dist, b4.REF.dist, b5.CC.dist, b5.OC.dist,
							 b5.REF.dist, b11.CC.dist, b11.OC.dist, b11.REF.dist, b18.CC.dist, 
							 b18.OC.dist, b18.REF.dist, b19.CC.dist, b19.OC.dist, b19.REF.dist,
							 b20.CC.dist, b20.OC.dist, b20.REF.dist, b37.CC.dist, b37.OC.dist, 
							 b37.REF.dist, b38.CC.dist, b38.OC.dist, b38.REF.dist))

close.dispersion |>
	group_by(TREATMENT) |>
	summarize(Dispersion = mean(DISTANCE))

close.dispersion$METER <- 'Close'

dispersion <- rbind(close.dispersion, far.dispersion)
rm(close.dispersion, far.dispersion)

# Clear the decks
rm(list=ls()[! ls() %in% c("centroid.dist", 'dispersion')])

# Check with models 
descdist(centroid.dist$DISTANCE)
centroid.mod <- lmer(DISTANCE~TREATMENT*METER + (1|BLOCK), data = centroid.dist)
Anova(centroid.mod)

descdist(dispersion$DISTANCE)
dispersion.mod <- lmer(DISTANCE~TREATMENT*METER + (1|BLOCK), data = dispersion)
Anova(dispersion.mod)

