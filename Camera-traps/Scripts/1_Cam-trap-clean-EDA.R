## --------------- HEADER ------------------------------------------------------
## Script name: 1_Cam-trap-clean-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-08-11
## Date Last Modified: 2023-08-01
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script cleans and explores the camera trap data for
## the SREL scavenger exclusion plots.

## --------------- SET-UP WORKSPACE --------------------------------------------
library(tidyverse)

cam.dat <- read.csv('Camera-traps/Raw-data/Exclusion-cam.csv')

# Two entries in the data had camera malfunctions
cam.dat[2,c(4:6)] <- NA
cam.dat[10,c(4:6)] <- NA
cam.dat[11,c(4:6)] <- NA

# Fix column names
colnames(cam.dat)[4] <- 'CARCASS.INTERACTIONS'
colnames(cam.dat)[5] <- 'CARCASS.PICTURES'

# Fix factor levels
cam.dat$TREATMENT[cam.dat$TREATMENT=='OC']<-"Open"
cam.dat$TREATMENT[cam.dat$TREATMENT=='CC']<-"Exclusion"

# Create a value for failures
cam.dat <- cam.dat |>
	mutate(NO.INTERACTIONS = CARCASS.PICTURES-CARCASS.INTERACTIONS)

# Rearrange the data
cam.dat <- cam.dat |>
	dplyr::select(SITE, PLOT, TREATMENT, CARCASS.INTERACTIONS,
								NO.INTERACTIONS, CARCASS.PICTURES, everything())

# Visualize data
cam.dat |>
	ggplot(aes(x = TREATMENT, y = CARCASS.PICTURES))+
	geom_point()+
	facet_wrap(~SITE)

cam.dat |>
	ggplot(aes(x = TREATMENT, y = CARCASS.INTERACTIONS))+
	geom_point()+
	facet_wrap(~SITE)

cam.dat |>
	ggplot(aes(x = TREATMENT, y = PROPORTION))+
	geom_point()+
	facet_wrap(~SITE)

write.csv(cam.dat, 'Camera-traps/Clean-data/Exclusion-cam.csv',
					row.names = FALSE)
