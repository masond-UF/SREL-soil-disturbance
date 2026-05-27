## --------------- HEADER ------------------------------------------------------
## Script name: 2_Soil-disturb-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-08-03
## Date Last Modified: 2023-08-02
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script cleans and explores the soil disturbance data 
## for the SREL scavenger exclusion plots.

# Load packages
library(tidyverse) # data manipulation
library(lubridate) # dates
library(lme4) # glmer()
library(car) # Anova()
library(performance) # check_model()
library(DHARMa) # model diagnostics
library(arm) # binnedplot()
library(emmeans) # emmeans() and emtrends()

# Clear the decks
rm(list=ls())

# Bring in the data
soil.disturb <- read.csv('Soil-disturbance/Clean-data/Soil-disturb')

# Convert distance into an ordinal variable?

# Add overdispersion parameter
soil.disturb$INDEX <- seq(1:nrow(soil.disturb))

# Convert exclusion and block to factor
soil.disturb$EXCLUSION <- as_factor(soil.disturb$EXCLUSION)
soil.disturb$BLOCK <- as_factor(soil.disturb$BLOCK)
soil.disturb$DATE.RD <- as_factor(soil.disturb$DATE.RD)

# Remove NAs
soil.disturb <- soil.disturb |>
	drop_na(SUCCESSES)

# Run the model
mod <- glmer(cbind(SUCCESSES, FAILURES) ~ EXCLUSION * DISTANCE * DATE.RD +
						 + (1|BLOCK) + (1|INDEX),
						 glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
						 family = binomial, data = soil.disturb)	
Anova(mod)
performance::check_model(mod)
check_singularity(mod) # False
summary(mod)

# Acceptable model fit (92% of residuals within confidence bands)
binnedplot(predict(mod, type="response", re.form=NA), 
					 resid(mod, type="response"), main='Without random effects', nclass=20)

mod.sim <- simulateResiduals(mod)
plot(mod.sim) # looks good

testOutliers(mod.sim, type = 'bootstrap') # ok

plotResiduals(mod.sim, soil.disturb$BLOCK, quantreg = T) # pass
plotResiduals(mod.sim, soil.disturb$EXCLUSION, quantreg = T) # pass
plotResiduals(mod.sim, soil.disturb$DISTANCE, quantreg = T) # pass
plotResiduals(mod.sim, soil.disturb$DATE.RD, quantreg = T) # pass

testDispersion(mod.sim) # ok
check_overdispersion(mod)
rm(mod.sim)

# Influential variables
4/length(unique(soil.disturb$BLOCK)) # 0.5 cook's D threshold
2/sqrt(length(unique(soil.disturb$BLOCK))) # 0.7 for params (Belsey, Kuh, Welsch) 
influential <- influence(mod, group="BLOCK")
car::infIndexPlot(influential) # row 38 is close to threshold for cooks
rm(influential)

# Adjust for variance
lme4::VarCorr(mod)
total.sd <- sqrt(2.18421^2 + 0.27699^2)

# Model outputs
emmeans(mod, pairwise~EXCLUSION, type = 'response', bias.adjust = TRUE,
				sigma = total.sd)
emmeans(mod, pairwise~EXCLUSION*DATE.RD, type = 'response', bias.adjust = TRUE,
				sigma = total.sd)
# (0.972-1) * 100 decrease 2.8%
# (0.972-1) * 100 decrease 2.8%

emtrends(mod, pairwise~EXCLUSION, var = 'DISTANCE',
				 bias.adjust = TRUE, sigma = total.sd)
exp(-1.255) # 0.29 caged  times the odds of having disturbance
# a 1 unit increase in meter increases the odds of soil disturbance by factor 0.29
# increased by decreased

exp(-1.255)/(1+exp(-1.255)) # 0.2218358
exp(-0.942) # 0.39 open  times the odds of having disturbance
exp(-0.942)/(1+exp(-0.942)) # 0.2804965 
# try the subtraction instead with probability

# function straight to the probability

# Predicted probability is decreasing as covariate increases.
# As the variable increases, the event is less likely to occur

emtrends(mod, pairwise~EXCLUSION*DATE.RD, var = 'DISTANCE',
				 bias.adjust = TRUE, sigma = total.sd, adjust = 'none',
				  at = list(DISTANCE = c(0, 1, 2, 3)))
exp(-1.624) # increase by a factor of 0.20 for each 1 meter in caged
# odds decrease by 0.20%
# (0.2-1) * 100 decrease by 80%

exp(-1.098) # increase by a factor of 0.33 for each 1 meter in caged
# odds decrease by 0.33
# (0.33-1) * 100 decrease 67%

# instead of multiplicative higher .22 do less than decrease

emmip(mod, EXCLUSION ~ DISTANCE, cov.reduce = range)

emmeans(mod, pairwise~EXCLUSION*DATE.RD, type = 'response', bias.adjust = TRUE,
				sigma = total.sd)
emtrends(mod, pairwise~EXCLUSION*DATE.RD, var = 'DISTANCE',
				 bias.adjust = TRUE, sigma = total.sd)
emmip(mod, EXCLUSION*DATE.RD ~ DISTANCE, cov.reduce = range)

rm(curWarnings)

# Predict proportion of disturbed cells
raster <- read.csv('Soil-disturbance/Raw-data/3_Raster.csv')

# Calculate distance for points
raster <- raster |>
	mutate(DISTANCE = sqrt(((x - 0)^2) + ((y-0)^2)))

mod <- glmer(cbind(SUCCESSES, FAILURES) ~ EXCLUSION * DISTANCE * DATE.RD +
						 + (1|BLOCK),
						 glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
						 family = binomial, data = soil.disturb)	

raster$DISTURBANCE.PROP <- predict(mod, raster, type = 'response')

# Combine dataframes
soil.disturb <- soil.disturb |>
	dplyr::select(-INDEX)

soil.disturb <- rbind(soil.disturb, raster)

write.csv(soil.disturb, 'Soil-disturbance/Clean-data/Soil-disturb-figures',
					row.names = FALSE)
