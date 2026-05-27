## --------------- HEADER ------------------------------------------------------
## Script name: 1_Soil-disturb-clean-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-08-03
## Date Last Modified: 2023-08-03
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script cleans and explores the soil disturbance data 
## for the SREL scavenger exclusion plots.

# Load packages
library(ggsci)
library(tidyverse)
library(lubridate)

# Clear the decks
rm(list=ls())

# Bring in the data
soil.disturb.2020 <- read.csv('Soil-disturbance/Raw-data/1_2020-12_Soil-disturb.csv')
soil.disturb.2022 <- read.csv('Soil-disturbance/Raw-data/2_2022-04_Soil-disturb.csv')

colnames(soil.disturb.2020)
colnames(soil.disturb.2022)

# Correct date in 2022 data
soil.disturb.2022 <- soil.disturb.2022 |>
	mutate(DATE = make_date(year = YEAR, month = MONTH, day = DAY))

# Drop extra date information in 2022
soil.disturb.2022 <- soil.disturb.2022 |>
	dplyr::select(-YEAR, -MONTH, -DAY)

# Correct date format in 2020
soil.disturb.2020$DATE <- mdy(soil.disturb.2020$DATE)

# Check class of plot
class(soil.disturb.2020$PLOT)
class(soil.disturb.2022$PLOT)
soil.disturb.2020$PLOT <- as_factor(soil.disturb.2020$PLOT)
soil.disturb.2022$PLOT <- as_factor(soil.disturb.2022$PLOT)

# Check class of cells
class(soil.disturb.2020$CELLS)
class(soil.disturb.2022$CELLS)

# Make sure cells and disturbance are similar formats
soil.disturb.2020$CELLS <- as.integer(soil.disturb.2020$CELLS)
soil.disturb.2022$CELLS <- as.integer(soil.disturb.2022$CELLS)
soil.disturb.2022$DISTURBANCE <- round(soil.disturb.2022$DISTURBANCE, 3)

class(soil.disturb.2020$DISTURBANCE)
class(soil.disturb.2022$DISTURBANCE)

# Combine the data and clear the environment
soil.disturb <- rbind(soil.disturb.2020, soil.disturb.2022)
1456/2
rm(soil.disturb.2020, soil.disturb.2022)

# Add failures for binary analysis
colnames(soil.disturb)[10] <- 'SUCCESSES'
soil.disturb <- soil.disturb |>
	mutate(FAILURES = 16-SUCCESSES)

# Check number of rows associated with each plot
# There should be 3 in each cardinal direction and 1
# in the center (n = 13)
soil.sum <- soil.disturb |>
	group_by(DATE, BLOCK, TREATMENT) |>
	summarize(n = n())
rm(soil.sum)# Checks out

# Create a date column that groups samples together
soil.disturb <- soil.disturb |>
	mutate(DATE.RD = round_date(DATE, unit = 'month')) |>
	dplyr::select(DATE, DATE.RD, everything())

# Check average soil disturbance percentage by exclusion
soil.disturb |>
	group_by(EXCLUSION) |>
	summarize(MEAN.DIST = mean(DISTURBANCE, na.rm = TRUE))

# Check average soil disturbance percentage by exclusion/year
soil.disturb |>
	group_by(DATE.RD, EXCLUSION) |>
	summarize(MEAN.DIST = mean(DISTURBANCE, na.rm = TRUE))

# Check average soil disturbance percentage by distance
soil.disturb |>
	group_by(DISTANCE) |>
	summarize(MEAN.DIST = mean(DISTURBANCE, na.rm = TRUE))

# Check average soil disturbance percentage by distance/exclusion
soil.disturb |>
	group_by(DISTANCE, EXCLUSION) |>
	summarize(MEAN.DIST = mean(DISTURBANCE, na.rm = TRUE))

# Check average soil disturbance percentage by date/distance
soil.disturb |>
	group_by(DATE.RD, DISTANCE) |>
	summarize(MEAN.DIST = mean(DISTURBANCE, na.rm = TRUE)) |>
	ggplot(aes(x = DISTANCE, y = MEAN.DIST))+
			geom_bar(stat = 'identity')+
			facet_wrap(~DATE.RD)

# Check average soil disturbance percentage by date/exclusion/distance
soil.disturb |>
	group_by(DATE.RD, EXCLUSION, DISTANCE) |>
	summarize(MEAN.DIST = mean(DISTURBANCE, na.rm = TRUE)) |>
	ggplot(aes(x = DISTANCE, y = MEAN.DIST, fill = EXCLUSION))+
			geom_bar(position = 'dodge', stat = 'identity')+
			facet_wrap(~DATE.RD)

# Check average soil disturbance percentage by exclusion/distance
soil.disturb |>
	group_by(EXCLUSION, DISTANCE) |>
	summarize(MEAN.DIST = mean(DISTURBANCE, na.rm = TRUE)) |>
	ggplot(aes(x = DISTANCE, y = MEAN.DIST, fill = EXCLUSION))+
			geom_bar(position = 'dodge', stat = 'identity')

# Add x,y data for spatial figure
soil.disturb$x <- NA
soil.disturb$y <- NA

for(i in 1:nrow(soil.disturb)){
	if(soil.disturb$TRANSECT[i] == 'CENTER'){
			soil.disturb$x[i] <- 0
			soil.disturb$y[i] <- 0
	}	
	if(soil.disturb$TRANSECT[i] == 'NORTH' & soil.disturb$DISTANCE[i] == 1){
			soil.disturb$x[i] <- 0
			soil.disturb$y[i] <- 1
	}
	if(soil.disturb$TRANSECT[i] == 'NORTH' & soil.disturb$DISTANCE[i] == 2){
			soil.disturb$x[i] <- 0
			soil.disturb$y[i] <- 2
	}
	if(soil.disturb$TRANSECT[i] == 'NORTH' & soil.disturb$DISTANCE[i] == 3){
			soil.disturb$x[i] <- 0
			soil.disturb$y[i] <- 3
	}
	if(soil.disturb$TRANSECT[i] == 'EAST' & soil.disturb$DISTANCE[i] == 1){
			soil.disturb$x[i] <- 1
			soil.disturb$y[i] <- 0
	}
	if(soil.disturb$TRANSECT[i] == 'EAST' & soil.disturb$DISTANCE[i] == 2){
			soil.disturb$x[i] <- 2
			soil.disturb$y[i] <- 0
	}
	if(soil.disturb$TRANSECT[i] == 'EAST' & soil.disturb$DISTANCE[i] == 3){
			soil.disturb$x[i] <- 3
			soil.disturb$y[i] <- 0
	}
	if(soil.disturb$TRANSECT[i] == 'SOUTH' & soil.disturb$DISTANCE[i] == 1){
			soil.disturb$x[i] <- 0
			soil.disturb$y[i] <- -1
	}
	if(soil.disturb$TRANSECT[i] == 'SOUTH' & soil.disturb$DISTANCE[i] == 2){
			soil.disturb$x[i] <- 0
			soil.disturb$y[i] <- -2
	}
	if(soil.disturb$TRANSECT[i] == 'SOUTH' & soil.disturb$DISTANCE[i] == 3){
			soil.disturb$x[i] <- 0
			soil.disturb$y[i] <- -3
	}
	if(soil.disturb$TRANSECT[i] == 'WEST' & soil.disturb$DISTANCE[i] == 1){
			soil.disturb$x[i] <- -1
			soil.disturb$y[i] <- 0
	}
	if(soil.disturb$TRANSECT[i] == 'WEST' & soil.disturb$DISTANCE[i] == 2){
			soil.disturb$x[i] <- -2
			soil.disturb$y[i] <- 0
	}
	if(soil.disturb$TRANSECT[i] == 'WEST' & soil.disturb$DISTANCE[i] == 3){
			soil.disturb$x[i] <- -3
			soil.disturb$y[i] <- 0
	}
}

# Change column name of DISTURBANCE
colnames(soil.disturb)[12] <- 'DISTURBANCE.PROP'

# Rearrange columns and drop some unnecessary columns
soil.disturb <- soil.disturb |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC') |>
	dplyr::select(DATE, DATE.RD, BLOCK, PLOT, EXCLUSION, TRANSECT, DISTANCE, x, y,
				 SUCCESSES, FAILURES, DISTURBANCE.PROP, NOTES)

# Visualize the spatial data
ggplot(soil.disturb, aes(x = x, y = y, fill = DISTURBANCE.PROP, size = DISTURBANCE.PROP))+
	geom_jitter(shape = 21, color = 'black', alpha = 0.5)+
	theme_classic()+
	ylab("")+
	xlab("")+
	theme(axis.text = element_blank(),
				axis.ticks = element_blank())+
	facet_wrap(~DATE.RD*EXCLUSION)

sum(is.na(soil.disturb$DISTURBANCE)) # Missing values

# Save the output as clean data
write.csv(soil.disturb, 'Soil-disturbance/Clean-data/Soil-disturb',
					row.names = FALSE)
