## --------------- HEADER ------------------------------------------------------
## Script name: 5_Disturbance-veg-figure.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-08-06
## Date Last Modified: 2023-08-06
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script analyzes the soil disturbance
## plant vegetation survey

## --------------- SET-UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidyr)

# Clear the decks
rm(list = ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	dplyr::filter(EXCLUSION != 'REF') |>
	dplyr::filter(TREATMENT == 'CC' | TREATMENT == 'OC') |>
	na.omit()

## --------------- CALCULATE TURNOVER ACROSS TREATMENT ------------------------

# We have empty rows
surv = surv[,colSums(surv[8:105]) > 0]
# For some reason colSums is missing values for JESS

# We need to add a column for BLANK so that BC will run
surv$BLANK <- 0
surv[105,79] <- 1
surv[56,79] <- 1

# Add ID tag
surv$ID <- paste(surv$BLOCK, surv$TREATMENT, 
								 surv$TRANSECT, surv$METER)

library(vegan)
# Block 11
d.11 <- surv |> 
	filter(BLOCK == 11)
braycurtis <- vegdist(d.11[,8:79])
meandist(braycurtis, d.11$ID)

d.11 <- tibble(
	BLOCK = rep(11, 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.74, 0.65, 0.77, 0.83, 0.43, 0.85, 1.00,
				 0.37, 0.58, 0.90, 0.71, 1.00, 1.00)
)

# Block 4
d.4 <- surv |> 
	filter(BLOCK == 4)
braycurtis <- vegdist(d.4[,8:79])
meandist(braycurtis, d.4$ID)

d.4 <- tibble(
	BLOCK = rep(4, 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(1.00, 0.89, 1.00, 1.00, 0.65, 0.95, 0.82,
				 0.76, 0.96, 0.93, 1.00, 0.60, 0.50)
)

# Block 5
d.5 <- surv |> 
	filter(BLOCK == 5)
braycurtis <- vegdist(d.5[,8:79])
meandist(braycurtis, d.5$ID)

d.5 <- tibble(
	BLOCK = rep(5, 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.58, 0.38, 0.68, 0.66, 0.33, 0.273, 0.84,
				 0.72, 0.74, 0.93, 0.43, 0.52, 0.88)
)

# Block 18
d.18 <- surv |> 
	filter(BLOCK == 18)
braycurtis <- vegdist(d.18[,8:79])
meandist(braycurtis, d.18$ID)

d.18 <- tibble(
	BLOCK = rep(18, 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.73, 0.95, 1.00, 0.79, 0.76, 0.80, 0.69,
				 0.69, 0.91, 0.81, 0.83, 0.57, 0.59)
)

# Block 19
d.19 <- surv |> 
	filter(BLOCK == 19)
braycurtis <- vegdist(d.19[,8:79])
meandist(braycurtis, d.19$ID)

d.19 <- tibble(
	BLOCK = rep(19, 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(1.00, 0.71, 0.67, 0.66, 0.64, 0.43, 0.89,
				 0.29, 0.33, 0.76, 0.45, 0.18, 0.31)
)

# Block 20
d.20 <- surv |> 
	filter(BLOCK == 20)
braycurtis <- vegdist(d.20[,8:79])
meandist(braycurtis, d.20$ID)

d.20 <- tibble(
	BLOCK = rep(20, 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(1.00, 0.87, 0.71, 0.53, 0.78, 0.84, 1.00,
				 1.00, 0.71, 0.92, 0.64, 0.73, 0.81)
)

# Block 37
d.37 <- surv |> 
	filter(BLOCK == 37)
braycurtis <- vegdist(d.37[,8:79])
meandist(braycurtis, d.37$ID)

d.37 <- tibble(
	BLOCK = rep(37, 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(1.00, 1.00, 0.29, 0.77, 0.83, 0.85, 0.80,
				 0.67, 1.00, 0.69, 0.78, 0.57, 0.92)
)

# Block 38
d.38 <- surv |> 
	filter(BLOCK == 38)
braycurtis <- vegdist(d.38[,8:79])
meandist(braycurtis, d.38$ID)

d.38 <- tibble(
	BLOCK = rep(38, 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.30, 0.90, 0.67, 0.41, 0.15, 0.45, 0.11,
				 0.62, 0.65, 0.63, 0.96, 0.64, 0.94)
)

trmt.turn <- rbind(d.11, d.18, d.19, d.20, d.37, d.38, d.4, d.5)


## --------------- CALCULATE TURNOVER ACROSS DISTANCE --------------------------

rm(list=ls()[! ls() %in% c("surv","trmt.turn")])

# Replicate the zero point sample so that each transect has it's own
# initial point before calculating BC

# Separate the zero points from the others
surv.zero <- surv |>
	filter(METER == 0)
surv.other <- surv |>
	filter(METER != 0)

# Copy them and correct the labels
surv.zero <- surv.zero |>
	slice(rep(1:n(), each = 4))
surv.zero$TRANSECT <- rep(c('NORTH', 'EAST', 'SOUTH', 'WEST'), 16)

# Bring the data back together
surv <- rbind(surv.zero, surv.other)
rm(surv.other, surv.zero)
256/16 # 16 points per plot (four cardinal directions with 4 points each)

# Pivot longer
surv.lg <- surv |>
	pivot_longer(8:79, names_to = 'SPECIES', values_to = 'STEMS')

surv.lg$ID <- paste(surv.lg$BLOCK, surv.lg$TREATMENT, surv.lg$TRANSECT)

# Calculate Bray-Curtis
library(codyn)
dist.turn <- turnover(df=surv.lg,  
               time.var = "METER", 
               species.var = "SPECIES",
               abundance.var = "STEMS", 
               replicate.var="ID")

# Parse out the pasted column
library(stringr)
dist.turn <- dist.turn |>
	separate(ID, c('BLOCK', 'TREATMENT', 'TRANSECT')) |>
	dplyr::select(BLOCK, TRANSECT, METER, total)
colnames(dist.turn)[4] <- 'BC'

rm(surv.lg)

## --------------- CALCULATE TURNOVER COMPARED TO REFERENCE --------------------

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	dplyr::filter(TREATMENT == 'CC' | TREATMENT == 'OC' |
									TREATMENT == 'REF') |>
	na.omit()

# We have empty rows
surv = surv[,colSums(surv[8:105]) > 0]
# For some reason colSums is missing values for JESS

# We need to add a column for BLANK so that BC will run
surv$BLANK <- 0
surv[105,79] <- 1
surv[56,79] <- 1

# Add ID tag
surv$ID <- paste(surv$TREATMENT, surv$TRANSECT, surv$METER)

# Rearrange
surv <- surv |>
	dplyr::select(BLOCK, PLOT, TREATMENT, EXCLUSION, ADDITION,
								TRANSECT, METER, ID, everything())

library(vegan)
# Block 4 OC
d.4.OC <- surv |> 
	filter(BLOCK == 4 & TREATMENT == 'OC' |
				 	BLOCK == 4 & TREATMENT == 'REF')
braycurtis <- vegdist(d.4.OC[,9:93])
meandist(braycurtis, d.4.OC$ID)

d.4.OC <- tibble(
	BLOCK = rep(4, 13),
	TREATMENT = rep('OC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.90, 0.60, 1.00, 0.96, 0.90, 0.87, 0.91,
				 0.94, 0.85, 0.94, 0.85, 0.94, 0.96)
)

# Block 4 CC
d.4.CC <- surv |> 
	filter(BLOCK == 4 & TREATMENT == 'CC' |
				 	BLOCK == 4 & TREATMENT == 'REF')
braycurtis <- vegdist(d.4.CC[,9:93])
meandist(braycurtis, d.4.CC$ID)

d.4.CC <- tibble(
	BLOCK = rep(4, 13),
	TREATMENT = rep('CC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.58, 1.00, 0.79, 0.78, 1.00, 0.53, 0.45,
				 0.87, 1.00, 0.57, 0.60, 1.00, 0.95)
)

# Block 5 OC
d.5.OC <- surv |> 
	filter(BLOCK == 5 & TREATMENT == 'OC' |
				 	BLOCK == 5 & TREATMENT == 'REF')
braycurtis <- vegdist(d.5.OC[,9:93])
meandist(braycurtis, d.5.OC$ID)

d.5.OC <- tibble(
	BLOCK = rep(5, 13),
	TREATMENT = rep('OC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.38, 0.82, 0.54, 0.63, 0.61, 0.40, 0.76,
				 0.41, 0.65, 1.00, 0.86, 0.68, 0.71)
)

# Block 5 CC
d.5.CC <- surv |> 
	filter(BLOCK == 5 & TREATMENT == 'CC' |
				 	BLOCK == 5 & TREATMENT == 'REF')
braycurtis <- vegdist(d.5.CC[,9:93])
meandist(braycurtis, d.5.CC$ID)

d.5.CC <- tibble(
	BLOCK = rep(5, 13),
	TREATMENT = rep('CC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.38, 0.88, 0.52, 0.90, 0.73, 0.47, 0.52,
				 0.61, 0.79, 0.70, 0.65, 0.50, 0.92)
)

# Block 11 OC
d.11.OC <- surv |> 
	filter(BLOCK == 11 & TREATMENT == 'OC' |
				 	BLOCK == 11 & TREATMENT == 'REF')
braycurtis <- vegdist(d.11.OC[,9:93])
meandist(braycurtis, d.11.OC$ID)

d.11.OC <- tibble(
	BLOCK = rep(11, 13),
	TREATMENT = rep('OC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.57, 0.56, 0.92, 1.00, 0.33, 0.87, 1.00,
				 0.43, 0.56, 0.60, 0.49, 0.85, 0.46)
)

# Block 11 CC
d.11.CC <- surv |> 
	filter(BLOCK == 11 & TREATMENT == 'CC' |
				 	BLOCK == 11 & TREATMENT == 'REF')
braycurtis <- vegdist(d.11.CC[,9:93])
meandist(braycurtis, d.11.CC$ID)

d.11.CC <- tibble(
	BLOCK = rep(11, 13),
	TREATMENT = rep('CC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.68, 0.52, 0.89, 1.00, 0.47, 0.71, 1.00,
				 0.33, 0.65, 1.00, 0.53, 1.00, 0.92)
)

# Block 18 OC
d.18.OC <- surv |> 
	filter(BLOCK == 18 & TREATMENT == 'OC' |
				 	BLOCK == 18 & TREATMENT == 'REF')
braycurtis <- vegdist(d.18.OC[,9:93])
meandist(braycurtis, d.18.OC$ID)

d.18.OC <- tibble(
	BLOCK = rep(18, 13),
	TREATMENT = rep('OC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.55, 1.00, 1.00, 0.64, 0.59, 0.97, 0.93,
				 0.87, 1.00, 0.95, 0.79, 0.61, 0.87)
)

# Block 18 CC
d.18.CC <- surv |> 
	filter(BLOCK == 18 & TREATMENT == 'CC' |
				 	BLOCK == 18 & TREATMENT == 'REF')
braycurtis <- vegdist(d.18.CC[,9:93])
meandist(braycurtis, d.18.CC$ID)

d.18.CC <- tibble(
	BLOCK = rep(18, 13),
	TREATMENT = rep('CC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.70, 0.77, 0.64, 0.90, 0.95, 0.51, 1.00,
				 0.73, 0.88, 0.84, 0.69, 0.33, 1.00)
)

# Block 19 OC
d.19.OC <- surv |> 
	filter(BLOCK == 19 & TREATMENT == 'OC' |
				 	BLOCK == 19 & TREATMENT == 'REF')
braycurtis <- vegdist(d.19.OC[,9:93])
meandist(braycurtis, d.19.OC$ID)

d.19.OC <- tibble(
	BLOCK = rep(19, 13),
	TREATMENT = rep('OC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.82, 0.26, 0.40, 0.49, 0.80, 0.68, 0.74,
				 0.85, 0.43, 0.83, 0.72, 0.62, 0.62)
)

# Block 19 CC
d.19.CC <- surv |> 
	filter(BLOCK == 19 & TREATMENT == 'CC' |
				 	BLOCK == 19 & TREATMENT == 'REF')
braycurtis <- vegdist(d.19.CC[,9:93])
meandist(braycurtis, d.19.CC$ID)

d.19.CC <- tibble(
	BLOCK = rep(19, 13),
	TREATMENT = rep('CC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(1.00, 0.61, 0.69, 0.44, 0.49, 0.59, 0.78,
				 0.95, 0.52, 0.88, 0.78, 0.51, 0.58)
)

# Block 20 OC
d.20.OC <- surv |> 
	filter(BLOCK == 20 & TREATMENT == 'OC' |
				 	BLOCK == 20 & TREATMENT == 'REF')
braycurtis <- vegdist(d.20.OC[,9:93])
meandist(braycurtis, d.20.OC$ID)

d.20.OC <- tibble(
	BLOCK = rep(20, 13),
	TREATMENT = rep('OC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.66, 0.81, 0.57, 0.94, 0.94, 0.60, 0.83,
				 0.93, 0.83, 0.67, 0.53, 0.88, 0.82)
)

# Block 20 CC
d.20.CC <- surv |> 
	filter(BLOCK == 20 & TREATMENT == 'CC' |
				 	BLOCK == 20 & TREATMENT == 'REF')
braycurtis <- vegdist(d.20.CC[,9:93])
meandist(braycurtis, d.20.CC$ID)

d.20.CC <- tibble(
	BLOCK = rep(20, 13),
	TREATMENT = rep('CC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.78, 1.00, 0.83, 0.88, 0.611, 0.74, 0.69,
				 0.73, 0.73, 0.84, 0.78, 0.91, 0.60)
)

# Block 37 OC
d.37.OC <- surv |> 
	filter(BLOCK == 37 & TREATMENT == 'OC' |
				 	BLOCK == 37 & TREATMENT == 'REF')
braycurtis <- vegdist(d.37.OC[,9:93])
meandist(braycurtis, d.37.OC$ID)

d.37.OC <- tibble(
	BLOCK = rep(37, 13),
	TREATMENT = rep('OC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(1.00, 0.50, 0.60, 0.81, 0.76, 0.89, 1.00,
				 0.70, 0.67, 0.58, 1.00, 0.81, 0.83)
)

# Block 37 CC
d.37.CC <- surv |> 
	filter(BLOCK == 37 & TREATMENT == 'CC' |
				 	BLOCK == 37 & TREATMENT == 'REF')
braycurtis <- vegdist(d.37.CC[,9:93])
meandist(braycurtis, d.37.CC$ID)

d.37.CC <- tibble(
	BLOCK = rep(37, 13),
	TREATMENT = rep('CC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(1.00, 1.00, 0.63, 1.00, 1.00, 0.81, 1.00,
				 0.56, 1.00, 1.00, 0.71, 0.81, 0.89)
)

# Block 38 OC
d.38.OC <- surv |> 
	filter(BLOCK == 38 & TREATMENT == 'OC' |
				 	BLOCK == 38 & TREATMENT == 'REF')
braycurtis <- vegdist(d.38.OC[,9:93])
meandist(braycurtis, d.38.OC$ID)

d.38.OC <- tibble(
	BLOCK = rep(38, 13),
	TREATMENT = rep('OC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.51, 0.82, 0.90, 0.53, 0.43, 0.36, 0.58,
				 0.63, 0.42, 0.70, 0.94, 1.00, 1.00)
)

# Block 38 CC
d.38.CC <- surv |> 
	filter(BLOCK == 38 & TREATMENT == 'CC' |
				 	BLOCK == 38 & TREATMENT == 'REF')
braycurtis <- vegdist(d.38.CC[,9:93])
meandist(braycurtis, d.38.CC$ID)

d.38.CC <- tibble(
	BLOCK = rep(38, 13),
	TREATMENT = rep('CC', 13),
	TRANSECT = c('CENTER', 'NORTH', 'NORTH', 'NORTH',
							 'EAST', 'EAST', 'EAST',
							 'SOUTH', 'SOUTH', 'SOUTH',
							 'WEST', 'WEST', 'WEST'),
	METER = c(0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
	BC = c(0.48, 0.42, 0.32, 0.20, 0.33, 0.29, 0.56,
				 0.10, 0.41, 0.82, 0.44, 1.00, 0.36)
)

ref.turn <- rbind(d.11.CC, d.11.OC, d.18.CC, d.18.OC, d.19.CC, d.19.OC,
									d.20.CC, d.20.OC, d.37.CC, d.37.OC, d.38.CC, d.38.OC,
									d.4.CC, d.4.OC, d.5.CC, d.5.OC)

rm(list=ls()[! ls() %in% c("trmt.turn", "dist.turn", "ref.turn", "surv")])

## --------------- ADD XY VALUES TO DATAFRAMES ---------------------------------

# Treatment turnover
trmt.turn$x <- NA
trmt.turn$y <- NA

for(i in 1:nrow(trmt.turn)){
	if(trmt.turn$TRANSECT[i] == 'CENTER'){
			trmt.turn$x[i] <- 0
			trmt.turn$y[i] <- 0
	}	
	if(trmt.turn$TRANSECT[i] == 'NORTH' & trmt.turn$METER[i] == 1){
			trmt.turn$x[i] <- 0
			trmt.turn$y[i] <- 1
	}
	if(trmt.turn$TRANSECT[i] == 'NORTH' & trmt.turn$METER[i] == 2){
			trmt.turn$x[i] <- 0
			trmt.turn$y[i] <- 2
	}
	if(trmt.turn$TRANSECT[i] == 'NORTH' & trmt.turn$METER[i] == 3){
			trmt.turn$x[i] <- 0
			trmt.turn$y[i] <- 3
	}
	if(trmt.turn$TRANSECT[i] == 'EAST' & trmt.turn$METER[i] == 1){
			trmt.turn$x[i] <- 1
			trmt.turn$y[i] <- 0
	}
	if(trmt.turn$TRANSECT[i] == 'EAST' & trmt.turn$METER[i] == 2){
			trmt.turn$x[i] <- 2
			trmt.turn$y[i] <- 0
	}
	if(trmt.turn$TRANSECT[i] == 'EAST' & trmt.turn$METER[i] == 3){
			trmt.turn$x[i] <- 3
			trmt.turn$y[i] <- 0
	}
	if(trmt.turn$TRANSECT[i] == 'SOUTH' & trmt.turn$METER[i] == 1){
			trmt.turn$x[i] <- 0
			trmt.turn$y[i] <- -1
	}
	if(trmt.turn$TRANSECT[i] == 'SOUTH' & trmt.turn$METER[i] == 2){
			trmt.turn$x[i] <- 0
			trmt.turn$y[i] <- -2
	}
	if(trmt.turn$TRANSECT[i] == 'SOUTH' & trmt.turn$METER[i] == 3){
			trmt.turn$x[i] <- 0
			trmt.turn$y[i] <- -3
	}
	if(trmt.turn$TRANSECT[i] == 'WEST' & trmt.turn$METER[i] == 1){
			trmt.turn$x[i] <- -1
			trmt.turn$y[i] <- 0
	}
	if(trmt.turn$TRANSECT[i] == 'WEST' & trmt.turn$METER[i] == 2){
			trmt.turn$x[i] <- -2
			trmt.turn$y[i] <- 0
	}
	if(trmt.turn$TRANSECT[i] == 'WEST' & trmt.turn$METER[i] == 3){
			trmt.turn$x[i] <- -3
			trmt.turn$y[i] <- 0
	}
}

# Distance turnover
dist.turn$x <- NA
dist.turn$y <- NA

for(i in 1:nrow(dist.turn)){
	if(dist.turn$TRANSECT[i] == 'CENTER'){
			dist.turn$x[i] <- 0
			dist.turn$y[i] <- 0
	}	
	if(dist.turn$TRANSECT[i] == 'NORTH' & dist.turn$METER[i] == 1){
			dist.turn$x[i] <- 0
			dist.turn$y[i] <- 1
	}
	if(dist.turn$TRANSECT[i] == 'NORTH' & dist.turn$METER[i] == 2){
			dist.turn$x[i] <- 0
			dist.turn$y[i] <- 2
	}
	if(dist.turn$TRANSECT[i] == 'NORTH' & dist.turn$METER[i] == 3){
			dist.turn$x[i] <- 0
			dist.turn$y[i] <- 3
	}
	if(dist.turn$TRANSECT[i] == 'EAST' & dist.turn$METER[i] == 1){
			dist.turn$x[i] <- 1
			dist.turn$y[i] <- 0
	}
	if(dist.turn$TRANSECT[i] == 'EAST' & dist.turn$METER[i] == 2){
			dist.turn$x[i] <- 2
			dist.turn$y[i] <- 0
	}
	if(dist.turn$TRANSECT[i] == 'EAST' & dist.turn$METER[i] == 3){
			dist.turn$x[i] <- 3
			dist.turn$y[i] <- 0
	}
	if(dist.turn$TRANSECT[i] == 'SOUTH' & dist.turn$METER[i] == 1){
			dist.turn$x[i] <- 0
			dist.turn$y[i] <- -1
	}
	if(dist.turn$TRANSECT[i] == 'SOUTH' & dist.turn$METER[i] == 2){
			dist.turn$x[i] <- 0
			dist.turn$y[i] <- -2
	}
	if(dist.turn$TRANSECT[i] == 'SOUTH' & dist.turn$METER[i] == 3){
			dist.turn$x[i] <- 0
			dist.turn$y[i] <- -3
	}
	if(dist.turn$TRANSECT[i] == 'WEST' & dist.turn$METER[i] == 1){
			dist.turn$x[i] <- -1
			dist.turn$y[i] <- 0
	}
	if(dist.turn$TRANSECT[i] == 'WEST' & dist.turn$METER[i] == 2){
			dist.turn$x[i] <- -2
			dist.turn$y[i] <- 0
	}
	if(dist.turn$TRANSECT[i] == 'WEST' & dist.turn$METER[i] == 3){
			dist.turn$x[i] <- -3
			dist.turn$y[i] <- 0
	}
}

# Reference turnover
ref.turn$x <- NA
ref.turn$y <- NA

for(i in 1:nrow(ref.turn)){
	if(ref.turn$TRANSECT[i] == 'CENTER'){
			ref.turn$x[i] <- 0
			ref.turn$y[i] <- 0
	}	
	if(ref.turn$TRANSECT[i] == 'NORTH' & ref.turn$METER[i] == 1){
			ref.turn$x[i] <- 0
			ref.turn$y[i] <- 1
	}
	if(ref.turn$TRANSECT[i] == 'NORTH' & ref.turn$METER[i] == 2){
			ref.turn$x[i] <- 0
			ref.turn$y[i] <- 2
	}
	if(ref.turn$TRANSECT[i] == 'NORTH' & ref.turn$METER[i] == 3){
			ref.turn$x[i] <- 0
			ref.turn$y[i] <- 3
	}
	if(ref.turn$TRANSECT[i] == 'EAST' & ref.turn$METER[i] == 1){
			ref.turn$x[i] <- 1
			ref.turn$y[i] <- 0
	}
	if(ref.turn$TRANSECT[i] == 'EAST' & ref.turn$METER[i] == 2){
			ref.turn$x[i] <- 2
			ref.turn$y[i] <- 0
	}
	if(ref.turn$TRANSECT[i] == 'EAST' & ref.turn$METER[i] == 3){
			ref.turn$x[i] <- 3
			ref.turn$y[i] <- 0
	}
	if(ref.turn$TRANSECT[i] == 'SOUTH' & ref.turn$METER[i] == 1){
			ref.turn$x[i] <- 0
			ref.turn$y[i] <- -1
	}
	if(ref.turn$TRANSECT[i] == 'SOUTH' & ref.turn$METER[i] == 2){
			ref.turn$x[i] <- 0
			ref.turn$y[i] <- -2
	}
	if(ref.turn$TRANSECT[i] == 'SOUTH' & ref.turn$METER[i] == 3){
			ref.turn$x[i] <- 0
			ref.turn$y[i] <- -3
	}
	if(ref.turn$TRANSECT[i] == 'WEST' & ref.turn$METER[i] == 1){
			ref.turn$x[i] <- -1
			ref.turn$y[i] <- 0
	}
	if(ref.turn$TRANSECT[i] == 'WEST' & ref.turn$METER[i] == 2){
			ref.turn$x[i] <- -2
			ref.turn$y[i] <- 0
	}
	if(ref.turn$TRANSECT[i] == 'WEST' & ref.turn$METER[i] == 3){
			ref.turn$x[i] <- -3
			ref.turn$y[i] <- 0
	}
}
## --------------- MODEL BRAY-CURTIS -------------------------------------------

N = nrow(ref.turn)
s <- 0.5

ref.turn <- ref.turn |>
	mutate(BC.trans = (BC*(N-1)+s)/N)

center.ref.turn <- ref.turn |>
	filter(METER == 0 | METER == 1)
outside.ref.turn <- ref.turn |>
	filter(METER == 2 | METER == 3)

descdist(center.ref.turn$BC, discrete = FALSE)
descdist(outside.ref.turn$BC, discrete = FALSE)

library(glmmTMB)

all.mod <- glmmTMB(BC.trans ~ TREATMENT * METER + (1|BLOCK), family = beta_family(link="logit"),
											data = ref.turn)
Anova(all.mod)

center.mod <- glmmTMB(BC.trans ~ TREATMENT * METER + (1|BLOCK), family = beta_family(link="logit"),
											data = center.ref.turn)
Anova(center.mod)

outside.mod <- glmmTMB(BC.trans ~ TREATMENT * METER + (1|BLOCK), family = beta_family(link="logit"),
											 data = outside.ref.turn)
Anova(outside.mod)

# Could try by grouping or focusing on other species.
# Could just treat the whole 2-3 m as a community and get beta.

## --------------- USE MODEL TO PREDICT VALUES ---------------------------------

# Bring in the blank dfs for prediction inputs
trmt.turn.pred <- read.csv('Plant-surveys/Raw-data/4_Trmt-turn-pred.csv')
dist.turn.pred <- read.csv('Plant-surveys/Raw-data/5_Dist-turn-pred.csv')
ref.turn.pred <- read.csv('Plant-surveys/Raw-data/6_Ref-turn-pred.csv')

# Check the distribution
library(fitdistrplus)
descdist(trmt.turn$BC, discrete = FALSE)
descdist(dist.turn$BC, discrete = FALSE)
descdist(ref.turn$BC, discrete = FALSE)

# Conduct the treatment turnover model
library(lme4)
trmt.turn.mod <- lmer(BC ~ METER + (1|BLOCK),
											data = trmt.turn)
Anova(trmt.turn.mod)

# Check the treatment turnover model
library(performance)
check_model(trmt.turn.mod) # looks good

# Predict Bray-Curtis values
trmt.turn.pred$BC <- predict(trmt.turn.mod, trmt.turn.pred)

# Combine predicted values with existing data
trmt.turn.pred <- rbind(trmt.turn, trmt.turn.pred)

# Export
write.csv(trmt.turn.pred, 
					'Plant-surveys/Clean-data/Treatment-turnover-fig.csv',
					row.names = FALSE)

# Conduct the distance turnover model
dist.turn.mod <- lmer(BC ~ METER + (1|BLOCK),
											data = dist.turn)
Anova(dist.turn.mod)

# Check the distance turnover model
check_model(dist.turn.mod) # looks good

# Predict Bray-Curtis values
dist.turn.pred$BC <- predict(dist.turn.mod, dist.turn.pred)

# Combine predicted values with existing data
dist.turn.pred <- rbind(dist.turn, dist.turn.pred)

# Export
write.csv(dist.turn.pred, 
					'Plant-surveys/Clean-data/Distance-turnover-fig.csv',
					row.names = FALSE)

# Conduct the reference turnover model
ref.turn.mod <- lmer(BC ~ METER * TREATMENT + (1|BLOCK),
											data = ref.turn)
Anova(ref.turn.mod)

# Check the reference turnover model
check_model(ref.turn.mod) # looks good

# Predict Bray-Curtis values
ref.turn.pred$BC <- predict(ref.turn.mod, ref.turn.pred)

# Combine predicted values with existing data
ref.turn.pred <- rbind(ref.turn.pred, ref.turn)

# Export
write.csv(ref.turn.pred, 
					'Plant-surveys/Clean-data/Reference-turnover-fig.csv',
					row.names = FALSE)

## --------------- CREATE THE FIGURE -------------------------------------------             

rm(list=ls()[! ls() %in% c("trmt.turn.pred", "dist.turn.pred",
													 "ref.turn.pred")])

# Treatment turnover 

# Get the mean value at each point
trmt.turn.pred <- trmt.turn.pred |>
	group_by(x, y) |>
	summarize(BC = mean(BC, na.rm = TRUE))

# Add 3 to remove negative values for matrix conversion
trmt.turn.pred$x <- trmt.turn.pred$x+3
trmt.turn.pred$y <- trmt.turn.pred$y+3

# Create wide data
trmt.turn.pred <- trmt.turn.pred |>
	spread(key = y, value = BC)

# Drop the x axis because we created row names
trmt.turn.pred <- trmt.turn.pred |> 
	ungroup() |>
	dplyr::select(-x)

# Convert to matrix
trmt.turn.pred <- as.matrix(trmt.turn.pred)

# Generate and export contour figure
library(plotly)
fig <- plot_ly(z = ~trmt.turn.pred, type = "contour",
							 zmin = 0.5, zmax = 1, height = 500, width = 630,
							 contours=list(
            start=0.5,
            end=1,
            size=0.05
        )) |>
	layout(title = 'Treatment turnover')
fig

export(fig, 'Plant-surveys/Figures/Treatment-turnover.png')
# Sites are more similar with more disturbance

# Distance turnover 

# Get the mean value at each point
dist.turn.pred <- dist.turn.pred |>
	group_by(x, y) |>
	summarize(BC = mean(BC, na.rm = TRUE))

# Add 3 to remove negative values for matrix conversion
dist.turn.pred$x <- dist.turn.pred$x+3
dist.turn.pred$y <- dist.turn.pred$y+3

# Create wide data
dist.turn.pred <- dist.turn.pred |>
	spread(key = y, value = BC)

# Drop the x axis because we created row names
dist.turn.pred <- dist.turn.pred |> 
	ungroup() |>
	dplyr::select(-x)

# Convert to matrix
dist.turn.pred <- as.matrix(dist.turn.pred)

# Generate and export contour figure
fig <- plot_ly(z = ~dist.turn.pred, type = "contour",
							 zmin = 0.5, zmax = 1, height = 500, width = 630,
							 contours=list(
            start=0.5,
            end=1,
            size=0.05
        )) |>
	layout(title = 'Distance turnover')
fig

export(fig, 'Plant-surveys/Figures/Distance-turnover.png')

# Reference turnover 
CC.ref <- ref.turn.pred |>
	filter(TREATMENT == 'CC')
OC.ref <- ref.turn.pred |>
	filter(TREATMENT == 'OC')

# Get the mean value at each point
CC.ref <- CC.ref |>
	group_by(x, y) |>
	summarize(BC = mean(BC, na.rm = TRUE))

OC.ref <- OC.ref |>
	group_by(x, y) |>
	summarize(BC = mean(BC, na.rm = TRUE))

# Add 3 to remove negative values for matrix conversion
CC.ref$x <- CC.ref$x+3
CC.ref$y <- CC.ref$y+3

OC.ref$x <- OC.ref$x+3
OC.ref$y <- OC.ref$y+3

# Create wide data
CC.ref <- CC.ref |>
	spread(key = y, value = BC)

OC.ref <- OC.ref |>
	spread(key = y, value = BC)

# Drop the x axis because we created row names
CC.ref <- CC.ref |> 
	ungroup() |>
	dplyr::select(-x)

OC.ref <- OC.ref |> 
	ungroup() |>
	dplyr::select(-x)

# Convert to matrix
CC.ref <- as.matrix(CC.ref)
OC.ref <- as.matrix(OC.ref)

# Generate and export contour figure
fig <- plot_ly(z = ~CC.ref, type = "contour",
							 zmin = 0.5, zmax = 0.1, height = 500, width = 600,
							 contours=list(
            start=0.5,
            end=1,
            size=0.05
        )) |>
	layout(title = 'CC reference turnover')
fig

export(fig, 'Plant-surveys/Figures/CC-Refence-turnover.png',
			 size)

fig <- plot_ly(z = ~OC.ref, type = "contour",
							 zmin = 0.65, zmax = 0.85, height = 500, width = 600,
							 contours=list(
            start=0.65,
            end=0.85,
            size=0.02
        )) |>
	layout(title = 'OC reference turnover')
fig
export(fig, 'Plant-surveys/Figures/OC-Refence-turnover.png')
