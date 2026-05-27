library(vegan)
library(tidyverse)
rm(list=ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'OC' | TREATMENT == 'REF' | TREATMENT == 'CC') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

surv <- surv |>
	pivot_longer(8:105, names_to = 'SPECIES', values_to = 'STEMS') |>
	group_by(BLOCK, TREATMENT, SPECIES) |>
	summarize(STEMS = sum(STEMS)) |>
	pivot_wider(names_from = SPECIES, values_from = STEMS)

spec <- surv[,3:100]
pred <- surv[,1:2]

mds <- metaMDS(spec, n = 100)  #using all the defaults
plot(mds, type = "t")

data.scores <- as.data.frame(vegan::scores(mds)$sites) 
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$treat <- pred$TREATMENT  #  add the grp variable created earlier
data.scores$block <- pred$BLOCK  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

data.scores$treat <- fct_rev(data.scores$treat)

ggplot() + 
  #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,fill=treat),pch=21, size=4,
  					 stroke = 1) + # add the point markers
	geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=block),alpha=0.5) +  # add the species labels
  coord_equal() +
	# scale_fill_viridis_d(option = 'A')+
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

levels(data.scores$block)
