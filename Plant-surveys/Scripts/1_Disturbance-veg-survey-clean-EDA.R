## --------------- HEADER ------------------------------------------------------
## Script name: 1_Disturbance-veg-survey-clean-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-08-04
## Date Last Modified: 2023-08-04
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script cleans and explores the soil disturbance
## plant vegetation survey

## --------------- SET THE WORK SPACE ------------------------------------------

library(tidyverse)

# Clear the decks
rm(list=ls())

# Bring in the species
pre <- read.csv('Plant-surveys/Raw-data/2020-pre-survey.csv')
surv <- read.csv('Plant-surveys/Raw-data/3_Soil-disturb-plants.csv') |>
	filter(!SPECIES %in% c("CLRA", "MOSS", "CUD3"))

# Fix data classes
surv$STEMS <- as.integer(surv$STEMS)
surv$DATE <- mdy(surv$DATE)
surv$METER <- as.integer(surv$METER)
surv$YEAR <- year(surv$DATE)

## --------------- CLEAN SPECIES -----------------------------------------------

# Combine PIGR
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'RIGR'){
		surv$SPECIES[i] <- 'PIGR'
	}
}

# Combine Sedges
CAREX <- c('SEDGE', 'SEDGE4', 'CAREX')
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% CAREX){
		surv$SPECIES[i] <- 'CAREX'
	}
}

# Combine DIT and DITE
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'DIT'){
		surv$SPECIES[i] <- 'DITE'
	}
}

# Combine DICO and DICH
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'DICO' | surv$SPECIES[i] == 'DICH '){
		surv$SPECIES[i] <- 'DICH'
	}
}

# Combine some grasses
OTHER.GRASS <- c("G2", "GRASS", "PASP", "RYE", "BLUE", "NEEDLE",
								 "BIG BROME", "G4", "POVERTY GRASS", "POV", "unk GRASS SEED",
								 "G2 THIN", "G2 FAT", "BROME", "G3", "UNK GRASS SEED",
								 "POV GRASS", "THIN RYE GRASS", "THIN BLUE", "POV G", "THIN")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% OTHER.GRASS){
		surv$SPECIES[i] <- 'UNK.GRASS'
	}
}

# Combine ARIS and ARISTIDA
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'ARIS' | surv$SPECIES[i] == 'ARIST'){
		surv$SPECIES[i] <- 'ARISTIDA'
	}
}

# Combine some PLVI type Asters
ASTER.1 <- c("ASTER/PLUI", "ASTER/PLVI", "PLVI", "ASTER PIVI", "PLUI")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% ASTER.1){
		surv$SPECIES[i] <- 'ASTER.1'
	}
}

# Combine Amborisa
AMPS <- c('AMBROSIA', 'AMP', 'AMPS')
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% AMPS){
		surv$SPECIES[i] <- 'AMPS'
	}
}

# Combine woodsorrel
WOODSORREL <- c('WOOD SURDER', 'WOODSORREL', "WOODSORREL ")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% WOODSORREL){
		surv$SPECIES[i] <- 'WOODSORREL'
	}
}

# Combine AXILFLOWER
AXILFLOWER <- c('AXIL', 'AXILMENT')
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% AXILFLOWER){
		surv$SPECIES[i] <- 'AXILFLOWER'
	}
}

# Combine other ASTER
AXILFLOWER <- c('AXIL', 'AXILMENT')
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% AXILFLOWER){
		surv$SPECIES[i] <- 'AXILFLOWER'
	}
}

# Combine Desmodium and Clitoria
DESMO.CLITOR <- c("DES", "DES/CLIT", "CILT")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% DESMO.CLITOR){
		surv$SPECIES[i] <- 'DESMO.CLITOR'
	}
}

# Combine SISYR
SISYR <- c("SISY", "BLUE EYE")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% SISYR){
		surv$SPECIES[i] <- 'SISYR'
	}
}

# Combine other ASTER
OTHER.ASTER <- c("ASTER", "ASTER1", "ASTR", "RAZOR EDGE ASTER")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% OTHER.ASTER){
		surv$SPECIES[i] <- 'OTHER.ASTER'
	}
}

# Combine other ASTERa
ASTER.4 <- c("FAT WHITE ASTER", "FAT WHITE ASTR", "ATER3")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% ASTER.4){
		surv$SPECIES[i] <- 'ASTER.4'
	}
}

# Combine other ASTERa
ASTER.5 <- c("THIN ASTER LEAF", "THIN ASTER LED", "THN ASTER LED")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% ASTER.5){
		surv$SPECIES[i] <- 'ASTER.5'
	}
}

# Combine some unknown forbs
FORB.1 <- c("MEATY", "MEAT")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% FORB.1){
		surv$SPECIES[i] <- 'FORB.1'
	}
}

# Combine hairy lespedeza
LEHI <- c("HAIRY LES", "HLES")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% LEHI){
		surv$SPECIES[i] <- 'LEHI'
	}
}

# Combine other lespedeza
LESPED <- c("LES", "LES R FLOWE", "LES R")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% LESPED){
		surv$SPECIES[i] <- 'LESPED'
	}
}

# Add pines
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == "PINE "){
		surv$SPECIES[i] <- 'PINE'
	}
}

# Combine SMBO
SMBO <- c("YES SMBO", "SMBO")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% SMBO){
		surv$SPECIES[i] <- 'SMBO'
	}
}

# Combine yellow eyed grass
YELLOW.EYE <- c("YELLOW", "YELLOW EYE")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% YELLOW.EYE){
		surv$SPECIES[i] <- 'YELLOW.EYE'
	}
}

# Combine Geranium
GERANIUM <- c("GERANIUM", "GERANIM", "GERONTUM")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% GERANIUM){
		surv$SPECIES[i] <- 'GERANIUM'
	}
}

# Combine Lonicera
LOJA <- c("LOJA", "CONIURD LOJA")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% LOJA){
		surv$SPECIES[i] <- 'LOJA'
	}
}

# Combine some redvein observations into a forb
FORB.2 <- c("RED VEIN", "RED ONE", "RED VIEN", "RED VEN")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% FORB.2){
		surv$SPECIES[i] <- 'FORB.2'
	}
}

# Combine some oaks
RED.OAK <- c('Q. RED', 'RED OAK', 'QUFA')
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% RED.OAK){
		surv$SPECIES[i] <- 'RED.OAK'
	}
}

# Combine conyza
CONYZA <- c('CONYON', 'CONYS', "CONYA")
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% CONYZA){
		surv$SPECIES[i] <- 'CONYZA'
	}
}

# Combine ERIG
ERIG <- c('ERIG', 'ERIG*')
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] %in% ERIG){
		surv$SPECIES[i] <- 'ERIG'
	}
}

# Check totals of species
spec.tot <- surv |>
	group_by(SPECIES) |>
	summarize(Total = sum(STEMS))
hist(spec.tot$Total)
quantile(spec.tot$Total, 0.05, na.rm =TRUE)

# Drop values with less than 1 count
# drop.list <- spec.tot |>
# 	filter(Total == 1) |>
# 	dplyr::select(-Total)
# drop.list <- as_vector(drop.list)
# 
# surv <- surv |>
# 	filter(!SPECIES %in% drop.list)

# Check totals again
spec.tot <- surv |>
	group_by(SPECIES) |>
	summarize(Total = sum(STEMS)) |>
	filter(!SPECIES %in% c('CLRA', 'CUD3', 'MOSS'))
hist(spec.tot$Total)

# Fix names
for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'ASTER3'){
		surv$SPECIES[i] <- 'ASTER.3'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'UNK SEED'){
		surv$SPECIES[i] <- 'FORB.3'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'WHORL'){
		surv$SPECIES[i] <- 'FORB.4'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'UNK DOR'){
		surv$SPECIES[i] <- 'FORB.5'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == '4EYE'){
		surv$SPECIES[i] <- 'FORB.6'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'FAT CUD'){
		surv$SPECIES[i] <- 'FORB.7'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'WIRE'){
		surv$SPECIES[i] <- 'FORB.8'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'RED STEM THIN LEAVES'){
		surv$SPECIES[i] <- 'FORB.9'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'HAIRY WHORL'){
		surv$SPECIES[i] <- 'FORB.10'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'FUZZY'){
		surv$SPECIES[i] <- 'FORB.11'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'LEAF FLOWER'){
		surv$SPECIES[i] <- 'FORB.12'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'WHITE CUD'){
		surv$SPECIES[i] <- 'FORB.13'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'OPP W'){
		surv$SPECIES[i] <- 'FORB.14'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'HAIRY OPP'){
		surv$SPECIES[i] <- 'FORB.15'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'MINT'){
		surv$SPECIES[i] <- 'FORB.16'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'TOOTH'){
		surv$SPECIES[i] <- 'FORB.17'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'WHORL2'){
		surv$SPECIES[i] <- 'FORB.18'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'THIN BROS BLE'){
		surv$SPECIES[i] <- 'UNK.PLANT.1'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'DKII'){
		surv$SPECIES[i] <- 'UNK.PLANT.2'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'SKELE'){
		surv$SPECIES[i] <- 'UNK.PLANT.3'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'BUSH'){
		surv$SPECIES[i] <- 'UNK.PLANT.4'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'SEED'){
		surv$SPECIES[i] <- 'UNK.PLANT.5'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'BLUE GOD'){
		surv$SPECIES[i] <- 'UNK.PLANT.6'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'PUV'){
		surv$SPECIES[i] <- 'UNK.PLANT.7'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == '2PSTEM'){
		surv$SPECIES[i] <- 'UNK.PLANT.8'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'PLUM LEAF'){
		surv$SPECIES[i] <- 'UNK.PLANT.9'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'UNK'){
		surv$SPECIES[i] <- 'UNK.PLANT.10'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'CUB'){
		surv$SPECIES[i] <- 'UNK.PLANT.11'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'FERN'){
		surv$SPECIES[i] <- 'UNK.PLANT.12'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'LONG SPATUSRE'){
		surv$SPECIES[i] <- 'UNK.PLANT.13'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'GALACTIA OTHER LES'){
		surv$SPECIES[i] <- 'UNK.FAB.1'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'ST JOHN'){
		surv$SPECIES[i] <- 'HYPER'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'SPURGE'){
		surv$SPECIES[i] <- 'EUPHORB'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'SPURGE'){
		surv$SPECIES[i] <- 'EUPHORB'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'WISTENIA'){
		surv$SPECIES[i] <- 'WIST'
	}
}

for(i in 1:nrow(surv)){
	if(surv$SPECIES[i] == 'SAMBULUS'){
		surv$SPECIES[i] <- 'SAMBUCUS'
	}
}

surv <- surv[surv$TREATMENT %in% c("CC", "OC", "REF"), ]

# Check totals again
spec.tot <- surv |>
	group_by(SPECIES) |>
	summarize(Total = sum(STEMS))
hist(spec.tot$Total)

## --------------- SAVE RICHNESS DF --------------------------------------------

library(dplyr)

## drop blanks/placeholders, filter to carrion + reference
surv.clean <- surv |>
  filter(!SPECIES %in% c("BLANK", "PLACEHOLDER"),
         TREATMENT %in% c("CC", "OC", "REF"))

## richness = number of distinct species with STEMS > 0 per quadrat
rich <- surv.clean |>
  group_by(DATE, BLOCK, PLOT, TREATMENT, EXCLUSION, TRANSECT, METER) |>
  summarize(RICHNESS = n_distinct(SPECIES), .groups = "drop")

## check for quadrats that had zero stems (they won't appear above)
## rebuild the full quadrat list and join
quadrats <- surv |>
  filter(TREATMENT %in% c("CC", "OC", "REF")) |>
  distinct(DATE, BLOCK, PLOT, TREATMENT, EXCLUSION, TRANSECT, METER)

rich <- left_join(quadrats, rich,
                  by = c("DATE", "BLOCK", "PLOT", "TREATMENT",
                         "EXCLUSION", "TRANSECT", "METER"))
rich$RICHNESS[is.na(rich$RICHNESS)] <- 0

## check
summary(rich$RICHNESS)
hist(rich$RICHNESS, breaks = 20, main = "Species richness per quadrat")
table(rich$TREATMENT, rich$RICHNESS == 0)

## save for analysis script
write.csv(rich, 'Plant-surveys/Clean-data/2_Richness.csv', row.names = FALSE)

## --------------- ADD GROWTH FORM ---------------------------------------------

# Create lists
grass <- c("UNK.GRASS", "ARISTIDA")

graminoid <- c("CAREX", "RUSH")

forb <- c("RUST",
          paste0("FORB.", 1:18),
          "ASTER.4", "ASTER.5", "ASTER2", "OTHER.ASTER",
          "CONYZA", "CROTON", "EUPHORB", "GERANIUM", "RUMEX", "WOODSORREL",
          "CHICKWEED", "DEER", "YELLOW.EYE", "TORS", "AMPS", "DESMO.CLITOR",
          "LESPED", "YELLOW CLOVER", "VICIA", "UNK.FAB.1", "AXILFLOWER",
          "ELEPH", "SISYR", "HYPER", "PARON", "MARSH PARSLEY", "NUCA",
          "JESS", "LEHI", "OPAX", "OPAX HAIRY", "OPUNT", "MIM", "HURW",
					"LIEF", "13EYE", "1PSTEM", "BIDEWD", "ERIO", "DELA", 
					"FLOWER LESOS", "LONG RETYOLE SOLN", "PICU", "PLUN", "VEAR",
					"WHITE RWY DILA", "SAMSUEL", "ROUND LEAF", "WIDRL", "YELLOW WHORL")

woody <- c("CEDAR", "RED.OAK", "HICKORY", "HAWTHORN",
           "VACCINIUM", "SAMBUCUS", "SMILAX", "VITIS", "WIST",
           "LIST", "TORA", "SMBO", "LOJA", "RHCO", "HACK", "QUIPH", "CARA")

drop <- c("BLANK", "PLACEHOLDER")

## species to keep as individual columns
keep.spp <- c("CUD", "DICH", "DITE", "ANDRO", "ASTER.1", "ASTER.3",
              "ERIG", "PIGR", "RUBUS", "PINE")

## assign hybrid category
surv$TAXA <- case_when(
  surv$SPECIES %in% keep.spp   ~ surv$SPECIES,
  surv$SPECIES %in% grass      ~ "OTHER.GRASS",
  surv$SPECIES %in% graminoid  ~ "OTHER.GRASS",
  surv$SPECIES %in% forb       ~ "OTHER.FORB",
  surv$SPECIES %in% woody      ~ "OTHER.WOODY",
  surv$SPECIES %in% drop       ~ "DROP",
  TRUE                         ~ "OTHER.UNKNOWN"
)
# Check totals
table(surv$TAXA, useNA = "always")

surv |>
 mutate(ZONE = case_when(
    METER == 0 ~ "center",
    METER == 1 ~ "near",
    TRUE       ~ "far"
  )) |>
	group_by(TREATMENT, ZONE, TAXA) |>
  summarize(Total.stems = sum(STEMS), .groups = "drop") |>
	filter(TAXA != 'DROP') |>
	ggplot(aes(x = TAXA, y = Total.stems))+
	geom_col()+
	theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
	facet_wrap(~TREATMENT*ZONE,ncol = 3)

	
## --------------- CREATE SPECIES MATRIX ---------------------------------------

# Create species matrix
surv.wd <- surv |>
	dplyr::select(-YEAR, -MONTH, -DAY, -STEMS..HIGH., -COVER, -NOTES, -LINE) |>
	group_by(DATE, BLOCK, PLOT, TREATMENT, EXCLUSION, ADDITION, TRANSECT, METER, TAXA) |>
	summarise(STEMS = sum(STEMS, na.rm = TRUE), .groups = "drop") |>
	pivot_wider(names_from = TAXA, values_from = STEMS) |>
	select(-DROP)

surv.wd[is.na(surv.wd)] <- 0

write.csv(surv.wd, 'Plant-surveys/Clean-data/3_Soil-disturb-func-plants.csv',
					row.names = FALSE)
