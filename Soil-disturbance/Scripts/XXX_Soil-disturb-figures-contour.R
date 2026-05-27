## --------------- HEADER ------------------------------------------------------
## Script name: 3_Soil-disturb-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-08-03
## Date Last Modified: 2023-08-03
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script visualizes the soil disturbance data 
## for the SREL scavenger exclusion plots.

library(tidyverse) # data manipulation
library(plotly) # plot_ly()

# Clear the decks
rm(list=ls())

# Bring in the data
soil.disturb <- read.csv('Soil-disturbance/Clean-data/Soil-disturb-figures')

# Get the mean value at each point
soil.disturb <- soil.disturb |>
	group_by(DATE.RD, EXCLUSION, x, y) |>
	summarize(DISTURBANCE.PROP = mean(DISTURBANCE.PROP, na.rm = TRUE))

# Add 3 to remove negative values for matrix conversion
soil.disturb$x <- soil.disturb$x+3
soil.disturb$y <- soil.disturb$y+3

# Caged 2020
df <- soil.disturb |>
	ungroup() |>
	filter(year(DATE.RD) == 2020 & EXCLUSION == 'CAGED') |>
	dplyr::select(x,y, DISTURBANCE.PROP) 

# Create wide data
df <- df |>
	spread(key = y, value = DISTURBANCE.PROP)

# Drop the x axis because we created row names
df <- df |> select(-x)

# Convert to matrix
df <- as.matrix(df)

# Generate and export contour figure
fig <- plot_ly(z = ~df, type = "contour",
							 zmin = 0, zmax = 0.9, height = 500, width = 600,
							 contours=list(
            start=0,
            end=0.9,
            size=0.1,
            widtth = 100
        )) |>
	layout(title = 'Caged 2020')

export(fig, 'Soil-disturbance/Figures/Countour-caged-2020.png')

# Open 2020 # 
df <- soil.disturb |>
	ungroup() |>
	filter(year(DATE.RD) == 2020 & EXCLUSION == 'OPEN') |>
	dplyr::select(x,y, DISTURBANCE.PROP) 

# Create wide data
df <- df |>
	spread(key = y, value = DISTURBANCE.PROP)

# Drop the x axis because we created row names
df <- df |> select(-x)

# Convert to matrix
df <- as.matrix(df)

# Generate and export contour figure
fig <- plot_ly(z = ~df, type = "contour",
							 zmin = 0, zmax = 0.9, height = 500, width = 600,
							 contours=list(
            start=0,
            end=0.9,
            size=0.1
        )) |>
	layout(title = 'Open 2020')

export(fig, 'Soil-disturbance/Figures/Countour-open-2020.png')

# Generate and export contour legend
fig <- plot_ly(z = ~df, type = "contour",
							 zmin = 0, zmax = 0.9,
							 contours=list(
            start=0,
            end=0.9,
            size=0.1
        )) |>
	layout(title = 'Open 2022')

export(fig, 'Soil-disturbance/Figures/Countour-legend.png')

# Caged 2022
df <- soil.disturb |>
	ungroup() |>
	filter(year(DATE.RD) == 2022 & EXCLUSION == 'CAGED') |>
	dplyr::select(x,y, DISTURBANCE.PROP) 

# Create wide data
df <- df |>
	spread(key = y, value = DISTURBANCE.PROP)

# Drop the x axis because we created row names
df <- df |> select(-x)

# Convert to matrix
df <- as.matrix(df)

# Generate and export contour figure
fig <- plot_ly(z = ~df, type = "contour",
							 zmin = 0, zmax = 0.9, height = 500, width = 600,
							 contours=list(
            start=0,
            end=0.9,
            size=0.1
        )) |>
	layout(title = 'Caged 2022')

export(fig, 'Soil-disturbance/Figures/Countour-caged-2022.png')

# Open 2022
df <- soil.disturb |>
	ungroup() |>
	filter(year(DATE.RD) == 2022 & EXCLUSION == 'OPEN') |>
	dplyr::select(x,y, DISTURBANCE.PROP) 

# Create wide data
df <- df |>
	spread(key = y, value = DISTURBANCE.PROP)

# Drop the x axis because we created row names
df <- df |> select(-x)

# Convert to matrix
df <- as.matrix(df)

# Generate and export contour figure
fig <- plot_ly(z = ~df, type = "contour",
							 zmin = 0, zmax = 0.9, height = 500, width = 600,
							 contours=list(
            start=0,
            end=0.9,
            size=0.1
        )) |>
	layout(title = 'Open 2022')

export(fig, 'Soil-disturbance/Figures/Countour-open-2022.png')

