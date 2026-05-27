## --------------- HEADER ------------------------------------------------------
## Script name: 4_Disturbance-veg-survey-analysis.R
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

library(tidyverse)


# Clear the decks
rm(list = ls())

surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	pivot_longer(cols = 8:105, names_to = 'SPECIES', values_to = 'COUNT')

surv$TRANSECT.METER <- paste(surv$TRANSECT, surv$METER)

surv <- surv |>
	group_by(BLOCK, TREATMENT, METER, TRANSECT.METER) |>
	summarize(TOT.COUNT = sum(COUNT))
	# 24 rows / 3 treatments / 4 sampling points = 8 blocks

# Check distribution
library(fitdistrplus)
descdist(surv$TOT.COUNT, discrete = TRUE) # poisson

# Run the model
library(lme4)
surv$INDEX <- seq(1:nrow(surv))
mod <- glmer(TOT.COUNT ~ TREATMENT*METER + (1|BLOCK) + (1|INDEX), 
						 family = 'poisson', data = surv,
						 control = glmerControl(optimizer ="bobyqa"))
library(car)
Anova(mod)

library(performance)
check_model(mod) # collinearity between treatment and treatment*meter
check_overdispersion(mod) 

library(emmeans)
emmeans(mod, pairwise~TREATMENT*METER, type = 'response',
				at = list(METER = 1,2,3))

ggplot(surv, aes(x = METER, y = TOT.COUNT, color = TREATMENT))+
	geom_jitter()+
	geom_smooth(method = "glm", , se = F, 
        method.args = list(family = "poisson"))
