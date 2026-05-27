
rm(list = ls())

# Bring in the data
surv <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv") |>
	filter(EXCLUSION == 'REF' | TREATMENT == 'CC' | TREATMENT == 'OC') |>
	filter(METER == 2 | METER == 3) |>
	na.omit()

surv <- surv[,colSums(surv[8:105]) > 5]

surv <- surv |>
	pivot_longer(cols = 8:46, names_to = 'SPECIES', values_to = 'COUNT')

surv <- surv |> 
	dplyr::filter(!SPECIES %in% c('CUD', 'DICH', 'DITE', 'OTHER.GRASS','PIGR', 
												'ASTER.3', 'ERIG', 'RUBUS', 'ANDRO', 'PINE', 'ASTER.1',
												 'DEER'))

list <- data_frame(unique(surv$SPECIES))

surv[surv == 0] <- NA

dev.off()
ggplot(surv, aes(x = COUNT, fill = TREATMENT))+
	geom_histogram(bins = 10)+
	facet_wrap(~SPECIES*TREATMENT, ncol = 3)+
	theme_bw()+
	theme(
  strip.background = element_blank(),
  strip.text.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = 'none'
)

ggplot(surv, aes(x = COUNT, fill = TREATMENT))+
	geom_histogram(bins = 20)+
	facet_wrap(~SPECIES*TREATMENT, ncol = 3, strip.position="right")+
	theme_bw()+
	theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = 'none',
  strip.text = element_text(size = 3)
)
