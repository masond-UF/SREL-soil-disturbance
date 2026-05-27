## --------------- HEADER ------------------------------------------------------
## Script name: 2_Cam-trap-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-08-11
## Date Last Modified: 2023-08-01
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script cleans and explores the camera trap data for
## the SREL scavenger exclusion plots.

# Load packages
library(ggsci)
library(tidyverse)

# Clear the decks
rm(list=ls())

# Bring in the data
cam.dat <- read.csv('Camera-traps/Clean-data/Exclusion-cam.csv')

# Set site and plot as factor
cam.dat$SITE <- as_factor(cam.dat$SITE)
cam.dat$PLOT <- as_factor(cam.dat$PLOT)

# Drop unneeded rows
cam.dat <- cam.dat |>
	dplyr::select(SITE, TREATMENT, CARCASS.INTERACTIONS, NO.INTERACTIONS)

# Pivot longer for bar plot
cam.dat <- cam.dat |>
	pivot_longer(cols = c(3,4), names_to = 'CATEGORY', values_to = 'COUNT')

# Reverse order of category
cam.dat$CATEGORY <- forcats::fct_rev(cam.dat$CATEGORY)

# Create new site names for figure
site_names <- c(
                    `4` = "1", 
                    `5` = "2",
                    `11` = "3",
                    `18` = "4",
                    `19` = "5",
                    `20` = "6",
                    `37` = "7",
                    `38` = "8"
   )

# Figure showing proportion of camera trap detections with carcass interactions
# at each site
ggplot(cam.dat, aes(fill=CATEGORY, y=COUNT, x=TREATMENT)) + 
    geom_bar(position="stack", stat="identity", color = 'black')+
		theme_classic()+
		xlab('')+
		ylab('Detections')+
		scale_y_continuous(expand = c(0,0),
											 limits = c(0,2250))+
		theme(legend.position = 'none')+
		theme(text = element_text(size=20),
					axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5,
																		 size = 14))+
		scale_fill_manual(values = c('white', 'black'))+
		facet_wrap(~SITE, nrow = 1,
							 labeller = as_labeller(site_names))

ggsave('Camera-traps/Figures/Cam-detections-sites.png',
			 height = 10, width = 10, units = 'in',
			 dpi = 300)

# Bring in the raw data
cam.dat <- read.csv('Camera-traps/Clean-data/Exclusion-cam.csv')

# Calculate mean % 
cam.dat <- cam.dat |>
	dplyr::group_by(TREATMENT) |>
	dplyr::summarize(MEAN.PROP = mean(PROPORTION, na.rm=TRUE),
						n = n(),
						se = sqrt((MEAN.PROP*(1-MEAN.PROP))/n))

# Visualize mean proportion of detections with carcass interactions
ggplot(cam.dat, aes(y=MEAN.PROP, x=TREATMENT, fill = TREATMENT)) + 
    geom_bar(position="stack", stat="identity")+
		theme_classic()+
		xlab('')+
		ylab('Proportion with carrion interaction')+
		scale_y_continuous(expand = c(0,0),
											 limits = c(0,1))+
		theme(legend.position = 'none')+
		theme(text = element_text(size=20),
					axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5,
																		 size = 14))+
		scale_fill_npg()

ggsave('Camera-traps/Figures/Cam-detections-means.png',
			 height = 10, width = 8, units = 'in',
			 dpi = 300)
