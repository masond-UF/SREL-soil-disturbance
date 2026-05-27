
library(tidyverse)
# Clear the decks
rm(list = ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(TREATMENT == 'CC' | TREATMENT == 'OC' | TREATMENT == 'REF') |>
	na.omit()

d <- surv |>
	pivot_longer(cols = 8:105, names_to = 'SPECIES', values_to = 'COUNT') |>
	group_by(SPECIES) |>
	summarize(MEAN = mean(COUNT))
rm(d)


for(i in 1:nrow(surv)){
	if(surv$METER[i] == 0 | surv$METER[i] == 1){
		surv$METER[i] <- 'Close'
	} else {
		surv$METER[i] <- 'Far'
	}
}

surv <- surv |>
	pivot_longer(8:105, names_to = 'SPECIES', values_to = 'COUNT')

surv <- surv |>
	group_by(BLOCK, TREATMENT, METER, SPECIES) |>
	summarize(COUNT = sum(COUNT))
	
surv.filt <- surv |> 
	dplyr::filter(SPECIES %in% c('CUD', 'DICH', 'DITE', 'OTHER.GRASS','PIGR', 
												'ASTER.3', 'ERIG', 'RUBUS', 'ANDRO', 'PINE', 'ASTER.1',
												 'DEER'))
rm(surv)

library(ggsci)
ggplot(data = surv.filt, aes(x = COUNT, y = SPECIES, fill = TREATMENT))+
	geom_jitter(shape = 21, size = 3, height = 0.15, color = 
								'black', alpha = 0.4)+
	scale_fill_npg()+
	facet_wrap(~METER*TREATMENT)+
	xlab('')+
	ylab('')+
	theme_bw()+
	theme(
  panel.grid.minor = element_blank(),
  legend.position = 'none',
  text = element_text(size = 15)
)

surv.filt |>
	group_by(SPECIES) |>
	summarize(TOTAL = sum(COUNT))

surv.filt$SPECIES <- factor(surv.filt$SPECIES, 
										levels = c('CUD', 'DICH', 'DITE', 'OTHER.GRASS','PIGR', 
												'ASTER.3', 'ERIG', 'RUBUS', 'ANDRO', 'PINE', 'ASTER.1',
												'FORB.3', 'DEER', 'FORB.5', 'AXILFLOWER'))


# Create the dataframe for the effect size figure

CC <- surv.filt |> filter(TREATMENT == 'CC')
OC <- surv.filt |> filter(TREATMENT == 'OC')
REF <- surv.filt |> filter(TREATMENT == 'REF')

REF <- REF[,5]
colnames(REF)[1] <- 'REF.COUNT'

CC <- cbind(CC, REF)
OC <- cbind(OC, REF)
es.df <- rbind(CC, OC)
rm(REF, CC, OC)

es.df <- es.df |>
	mutate(ES = log((COUNT+0.5)/(REF.COUNT+0.5)))

es.df <- es.df |>
	group_by(TREATMENT, METER, SPECIES) |>
	summarize(MEAN = mean(ES),
						sd = sd(ES),
						n = n(),
						se = sd/sqrt(n),
						margin = qt(0.975,df=n-1)*sd/sqrt(n),
						LCL = MEAN-margin,
						UCL = MEAN+margin)


es.df$SPECIES <- factor(es.df$SPECIES, 
										levels = c('CUD', 'ASTER.1', 'DITE', 'ANDRO',
															 'PINE', 'PIGR', 'ERIG', 'DEER', 'OTHER.GRASS',
															 'ASTER.3', 'DICH', 'RUBUS'))
es.df$SPECIES <- fct_rev(es.df$SPECIES)

library(ggsci)
ggplot(data = es.df, aes(x = MEAN, y = SPECIES, fill = TREATMENT))+
	geom_linerange(aes(xmin = LCL, xmax = UCL))+
	geom_point(shape = 21, size = 3, color = 
								'black')+
	scale_fill_npg()+
	facet_wrap(~METER*TREATMENT)+
	scale_x_continuous(limits=c(-3,3),
										 breaks = c(-3,-2,-1,0,1,2,3))+
	xlab('')+
	ylab('')+
	theme_bw()+
	theme(
  panel.grid.minor = element_blank(),
  legend.position = 'none',
  text = element_text(size = 15),
  # axis.text.y = element_blank(),
  # strip.background = element_blank(),
  # strip.text.x = element_blank()
)
