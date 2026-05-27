## --------------- HEADER ------------------------------------------------------
## Script name: 4b_Disturbance-veg-survey-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-08-05
## Date Last Modified: 2023-08-05
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script analyzes the soil disturbance
## plant vegetation survey

## --------------- SET-UP WORKSPACE --------------------------------------------

library(mvabund)
library(tidyverse)
library(fitdistrplus)

# Clear the decks
rm(list = ls())

## --------------- OLD MODEL WITH ZERO IN CENTER -----------------------------------

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(EXCLUSION != 'REF') |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC') |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

# Take the 10 most common species to speed up the analysis
# surv <- surv |>
#   select(
#     BLOCK, PLOT, TREATMENT, EXCLUSION, ADDITION, TRANSECT, METER,
#     CUD, DICH, DITE, OTHER.GRASS, PIGR,
#     ASTER.3, ERIG, RUBUS, ANDRO, PINE
#   )

# Convert to mvabund object
spp <- mvabund(surv[, 8:78])

# surv$BLOCK <- as_factor(surv$BLOCK)

# Run the model
mod1 <- manyglm(spp ~ surv$EXCLUSION * surv$METER, 
								family = "negative_binomial")
anova.manyglm(mod1)
anova(mod1, p.uni = "adjusted")
summary(mod1)
plot(mod1)

library(permute)
permID <- shuffleSet(n = nrow(surv), nset = 99, control = how(block = surv$BLOCK))
output <- anova(mod1, p.uni = "unadjusted", bootID = permID)


p.val <- tibble(output$uni.p)

## --------------- OLD MODEL WITH NO CENTER ----------------------------------------

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(EXCLUSION != 'REF') |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC') |>
	filter(TRANSECT != 'CENTER') |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

# Convert to mvabund object
spp <- mvabund(surv[, 8:76])

# Run the model
mod1 <- manyglm(spp ~ surv$EXCLUSION + surv$TRANSECT, 
								family = "negative_binomial")
anova.manyglm(mod1)
anova(mod1, p.uni = "adjusted")
summary(mod1)
plot(mod1)

library(permute)
permID <- shuffleSet(n = nrow(surv), nset = 99, control = how(block = surv$BLOCK))
output <- anova(mod1, p.uni = "unadjusted", bootID = permID)


p.val <- tibble(output$uni.p)


## --------------- OLD MODEL JUST OUTSIDE ------------------------------------------

set.seed(1)

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(EXCLUSION != 'REF') |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC') |>
	filter(METER == 3) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

# Convert to mvabund object
spp <- mvabund(surv[, 8:61])

# surv$BLOCK <- as_factor(surv$BLOCK)

# Run the model
mod1 <- manyglm(spp ~ surv$EXCLUSION, 
								family = "negative_binomial")
plot(mod1)

summary(mod1)

library(permute)
permID <- shuffleSet(n = nrow(surv), nset = 99, control = how(block = surv$BLOCK))
output <- anova(mod1, p.uni = "unadjusted", bootID = permID)

p.val <- tibble(output$uni.p)

## --------------- MODEL INSIDE CC --------------------------------------------

set.seed(1)

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'REF') |>
	filter(METER == 0 | METER == 1) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

# Convert to mvabund object
spp <- mvabund(surv[, 8:50])

# surv$BLOCK <- as_factor(surv$BLOCK)

# Run the model
mod1 <- manyglm(spp ~ surv$EXCLUSION * surv$METER, 
								family = "negative_binomial")
plot(mod1)
summary(mod1)

library(permute)
permID <- shuffleSet(n = nrow(surv), nset = 99, control = how(block = surv$BLOCK))
output <- anova(mod1, p.uni = "unadjusted", bootID = permID)

# p = 0.23

p.val <- tibble(output$uni.p)

## --------------- MODEL INSIDE OC --------------------------------------------

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 0 | METER == 1) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

# Convert to mvabund object
spp <- mvabund(surv[, 8:55])

# surv$BLOCK <- as_factor(surv$BLOCK)

# Run the model
mod1 <- manyglm(spp ~ surv$EXCLUSION * surv$METER, 
								family = "negative_binomial")
plot(mod1)
summary(mod1)

library(permute)
permID <- shuffleSet(n = nrow(surv), nset = 99, control = how(block = surv$BLOCK))
output <- anova(mod1, p.uni = "unadjusted", bootID = permID)

p.val <- tibble(output$uni.p)

mod1$
plot.mvabund(pp ~ TREATMENT)


## --------------- MODEL OUTSIDE CC --------------------------------------------

set.seed(1)

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'REF') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

# Convert to mvabund object
spp <- mvabund(surv[, 8:69])

# surv$BLOCK <- as_factor(surv$BLOCK)

# Run the model
mod1 <- manyglm(spp ~ surv$EXCLUSION * surv$METER, 
								family = "negative_binomial")
plot(mod1)
summary(mod1)

library(permute)
permID <- shuffleSet(n = nrow(surv), nset = 99, control = how(block = surv$BLOCK))
output <- anova(mod1, p.uni = "unadjusted", bootID = permID)

# Run 1 p = 0.08
# Run 2 p = 0.12
# Run 3 p = 0.15
# Run 4 p = 0.09
# Run 5 p = 0.10
model.pvals <- c(0.08, 0.12, 0.15, 0.09, 0.10)
mean(model.pvals) # 0.108

ind.p.val <- tibble(output$uni.p)

## --------------- MODEL OUTSIDE OC --------------------------------------------

set.seed(1)

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

# Convert to mvabund object
spp <- mvabund(surv[, 8:71])

# surv$BLOCK <- as_factor(surv$BLOCK)

# Run the model
mod1 <- manyglm(spp ~ surv$EXCLUSION * surv$METER, 
								family = "negative_binomial")
plot(mod1)
summary(mod1)

library(permute)
permID <- shuffleSet(n = nrow(surv), nset = 99, control = how(block = surv$BLOCK))
output <- anova(mod1, p.uni = "unadjusted", bootID = permID)

# Run 1 p = 0.01
# Run 2 p = 0.01
# Run 3 p = 0.01
# Run 4 p = 0.01
# Run 5 p = 0.01
model.pvals <- c(0.01, 0.01, 0.01, 0.01, 0.01)
mean(model.pvals) # 0.01**

ind.p.val <- tibble(output$uni.p)

mod1$
plot.mvabund(pp ~ TREATMENT)

## --------------- RICHNESS ----------------------------------------------------

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

# Maintaining all subsamples
rich <- surv |>
	pivot_longer(cols = 8:91, names_to = 'SPECIES', values_to = 'COUNT') |>
	group_by(BLOCK, TREATMENT, TRANSECT, METER, SPECIES) |>
	summarise(COUNT = sum(COUNT)) |>
	filter(COUNT > 0) |>
	group_by(BLOCK, TREATMENT, TRANSECT, METER) |>
	summarise(RICHNESS = n())

check <- rich |>
	group_by(BLOCK) |>
	summarize(n = n())
# Block 19 and 37 missing values

true.zeroes <- data_frame(BLOCK = c(19, 37), 
													TREATMENT = c('CC', 'REF'), 
													TRANSECT = c('CENTER', 'EAST'), 
													METER = c(0, 3), 
													RICHNESS = c(0, 0))
rich <- rbind(rich, true.zeroes)
rm(check, true.zeroes)

descdist(rich$RICHNESS, discrete = TRUE)

rich.mod <- lmer(RICHNESS ~ TREATMENT * METER + TRANSECT + (1|BLOCK),
								 data = rich)
library(car)
Anova(rich.mod, type = 2) # type matters

library(performance)
check_model(rich.mod)

emmeans(rich.mod, pairwise~TREATMENT*METER,
				at = list(METER = c(0,1,2,3)))
emmip(rich.mod, TREATMENT ~ METER, at = list(METER = c(0,1,2,3)),
			type = 'response')

emtrends(rich.mod, pairwise~TREATMENT, var = 'METER')

## --------------- DIVERSITY ---------------------------------------------------

# Clear the decks
rm(list=ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

library(vegan)
diversity <- diversity(surv[,8:91])

surv$DIVERSITY <- diversity

surv <- surv |>
	dplyr::select(BLOCK, TREATMENT, TRANSECT, METER, DIVERSITY)

descdist(surv$DIVERSITY, discrete = FALSE)

div.mod <- lmer(DIVERSITY ~ TREATMENT * METER + TRANSECT + (1|BLOCK),
								 data = surv)
library(car)
Anova(div.mod, type = 2) # type matters

library(performance)
check_model(div.mod)

library(DHARMa)
sim.mod <- simulateResiduals(div.mod)
plot(sim.mod)

emmeans(div.mod, pairwise~TREATMENT*METER,
				at = list(METER = c(0,1,2,3)))
emmip(rich.mod, TREATMENT ~ METER, at = list(METER = c(0,1,2,3)),
			type = 'response')

emtrends(rich.mod, pairwise~TREATMENT, var = 'METER')

## --------------- INDICATOR SPECIES CLOSE -------------------------------------

# Clear the decks
rm(list=ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 0 | METER == 1) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

spec <- surv[,8:61]

library(indicspecies)
indval <- multipatt(spec, surv$TREATMENT, 
                    control = how(nperm=999)) 
summary(indval)
rm(spec)

# Only difference is SISYR and YELLOW.EYE at OC

## --------------- INDICATOR SPECIES FAR ---------------------------------------

# Clear the decks
rm(list=ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]
surv <- surv[, !apply(surv == 0, 2, all)]

spec <- surv[,8:61]

library(indicspecies)
indval <- multipatt(spec, surv$TREATMENT, 
                    control = how(nperm=999)) 
summary(indval)
rm(spec)

# CC = FORB3
# OC = ASTER 4 abd FORB5
# REF = FORB1 and RUST

surv <- surv |>
	pivot_longer(cols = 8:61, names_to = 'SPECIES', values_to = 'COUNT')
surv$BLOCK <- as_factor(surv$BLOCK)

find.zero <- surv |>
	group_by(SPECIES) |>
	summarize(TOTAL = sum(COUNT))

spec.list <- as_vector(unique(surv$SPECIES))
plot.list <- list()

for(i in 1:length(spec.list)){

	spec <- spec.list[i]
	
	d <- surv |> filter(SPECIES == spec)

	p <- ggplot(d, aes(x = BLOCK, y = COUNT, fill = TREATMENT))+
				geom_jitter(shape = 21, width = 0.2, height = 0)+
				ggtitle(paste(spec))+
				theme_bw()+
				facet_wrap(~TREATMENT, ncol = 1)

	plot.list[[i]] <- p
}

pdf("Plant-surveys/Figures/Species-counts.pdf")
for (i in 1:length(plot.list)) {
    print(plot.list[[i]])

}
dev.off()


## --------------- OLD RICHNESS ------------------------------------------------

close.OC <- rich |> filter(TREATMENT != 'CC' & METER < 2)
close.CC <- rich |> filter(TREATMENT != 'OC' & METER < 2)

add <- data.frame(BLOCK = 19, 
							TREATMENT = c('REF'), TRANSECT = c('CENTER'),
							METER = c(0), RICHNESS = c(0))
close.CC <- rbind(close.CC, add)

close.OC.mod <- lmer(RICHNESS ~ TREATMENT + (1|BLOCK),
								 data = close.OC)
Anova(close.OC.mod)

close.CC.mod <- lmer(RICHNESS ~ TREATMENT + (1|BLOCK),
								 data = close.CC)
Anova(close.CC.mod)

far.OC <- rich |> filter(TREATMENT != 'CC' & METER > 1)
far.CC <- rich |> filter(TREATMENT != 'OC' & METER > 1)

far.OC.mod <- lmer(RICHNESS ~ TREATMENT + (1|BLOCK),
								 data = far.OC)
Anova(far.OC.mod)

far.CC.mod <- lmer(RICHNESS ~ TREATMENT + (1|BLOCK),
								 data = far.CC)
Anova(far.CC.mod)

# Grouping subsamples
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 0]

close.OC <- surv |> filter(TREATMENT != 'CC' & METER < 2)

close.OC <- close.OC |>
	pivot_longer(cols = 8:91, names_to = 'SPECIES', values_to = 'COUNT') |>
	group_by(BLOCK, TREATMENT, SPECIES) |>
	summarise(COUNT = sum(COUNT)) |>
	filter(COUNT > 0) |>
	group_by(BLOCK, TREATMENT) |>
	summarise(RICHNESS = n())

close.OC.mod <- lmer(RICHNESS ~ TREATMENT + (1|BLOCK),
								 data = close.OC)
Anova(close.OC.mod)

close.CC <- surv |> filter(TREATMENT != 'OC' & METER < 2)

close.CC <- close.CC |>
	pivot_longer(cols = 8:91, names_to = 'SPECIES', values_to = 'COUNT') |>
	group_by(BLOCK, TREATMENT, SPECIES) |>
	summarise(COUNT = sum(COUNT)) |>
	filter(COUNT > 0) |>
	group_by(BLOCK, TREATMENT) |>
	summarise(RICHNESS = n())

close.CC.mod <- lmer(RICHNESS ~ TREATMENT + (1|BLOCK),
								 data = close.CC)
Anova(close.CC.mod)

far.OC <- surv |> filter(TREATMENT != 'CC' & METER > 1)

far.OC <- far.OC |>
	pivot_longer(cols = 8:91, names_to = 'SPECIES', values_to = 'COUNT') |>
	group_by(BLOCK, TREATMENT, SPECIES) |>
	summarise(COUNT = sum(COUNT)) |>
	filter(COUNT > 0) |>
	group_by(BLOCK, TREATMENT) |>
	summarise(RICHNESS = n())

far.OC.mod <- lmer(RICHNESS ~ TREATMENT + (1|BLOCK),
								 data = far.OC)
Anova(far.OC.mod)

far.CC <- surv |> filter(TREATMENT != 'OC' & METER > 1)

far.CC <- far.CC |>
	pivot_longer(cols = 8:91, names_to = 'SPECIES', values_to = 'COUNT') |>
	group_by(BLOCK, TREATMENT, SPECIES) |>
	summarise(COUNT = sum(COUNT)) |>
	filter(COUNT > 0) |>
	group_by(BLOCK, TREATMENT) |>
	summarise(RICHNESS = n())

far.CC.mod <- lmer(RICHNESS ~ TREATMENT + (1|BLOCK),
								 data = far.CC)
Anova(far.CC.mod)

## --------------- OLD SINGLE SPP MODELS ---------------------------------------

# EXCL: cud, pigr (marginal)
# DISt : Dich, ASTER 3, ERIG

pred.df <- surv
pred.df$ADDITION <- NA
pred.df$METER <- NA
pred.df$TRANSECT <- NA
pred.df[, 8:17] <- NA

pred.df <- unique(pred.df)

# Add coordinates
pred.df$x <- list(list(
  1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, -1, -1, -1, -2, -2, -2, -3, -3, -3,
  -1, -1, -1, -2, -2, -2, -3, -3, -3
))
pred.df$y <- list(list(
  1, 2, 3, 1, 2, 3, -1, -2, -3, -1, -2, -3, -1, -2, -3, -1, -2, -3,
  1, 2, 3, 1, 2, 3, 1, 2, 3
))

pred.df <- pred.df |>
    unnest(x, .drop = FALSE) |>
    unnest(y, .drop = FALSE) |>
		unique()

pred.df$x <- as.numeric(pred.df$x)
pred.df$y <- as.numeric(pred.df$y)

pred.df <- pred.df |>
	mutate(METER = round(sqrt(((x - 0)^2) + ((y-0)^2)), 2)) |>
	dplyr::select(-x, -y)

# CUD model
pred.df <- pred.df[,1:7]

descdist(surv$CUD, discrete = TRUE)
cud.mod <- glmer.nb(CUD ~ EXCLUSION * METER + (1|BLOCK),
								 data = surv)
Anova(cud.mod)
check_model(cud.mod)

pred.df$CUD <- NA
pred.df$CUD <- predict(cud.mod, pred.df)
pred.df$CUD <- round(exp(pred.df$CUD))

# DICH model

descdist(surv$DICH, discrete = TRUE)
dich.mod <- glmer.nb(DICH ~ EXCLUSION * METER + (1|BLOCK),
								 data = surv)
Anova(dich.mod)
check_model(dich.mod)

pred.df$DICH <- NA
pred.df$DICH <- predict(dich.mod, pred.df)
pred.df$DICH <- round(exp(pred.df$DICH))

# ASTER3 model

descdist(surv$ASTER.3, discrete = TRUE)
aster.3.mod <- glmer.nb(ASTER.3 ~ EXCLUSION * METER + (1|BLOCK),
								 data = surv)
Anova(aster.3.mod)
check_model(aster.3.mod)

pred.df$DICH <- NA
pred.df$DICH <- predict(dich.mod, pred.df)
pred.df$DICH <- round(exp(pred.df$DICH))

