library(tidyverse)
library(vegan)

rm(list=ls())

plants <- read.csv("Plant-surveys/Clean-data/2_Soil-disturb-plants.csv")  |>
	filter(TREATMENT == "REF" | TREATMENT == "OC" | TREATMENT == "CC" ) |>
	filter(METER > 1)

species.cols <- names(plants)[8:ncol(plants)]

# Reference baseline
ref.base <- plants |>
  filter(TREATMENT == "REF") |>
  group_by(BLOCK, TRANSECT, METER) |>
  summarise(across(all_of(species.cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Join experimental plots to the reference baseline
plants <- plants |>
  filter(TREATMENT != "REF") |>
  left_join(ref.base, by = c("BLOCK", "TRANSECT", "METER"), suffix = c("", ".ref"))

for (sp in species.cols) {
  plants[[paste0("diff.", sp)]] <- plants[[sp]] - plants[[paste0(sp, ".ref")]]
}

# We filter out species appearing in < 5% of samples to avoid model noise
diff.mat <- plants |> dplyr::select(starts_with("diff."))
occurrence <- colSums(diff.mat != 0, na.rm = TRUE) / nrow(diff.mat)
keep.cols <- names(occurrence)[occurrence > 0.05]

# Merge back predictor data with reference difference
plants <- plants |> 
  dplyr::select(BLOCK, PLOT, TREATMENT, EXCLUSION, TRANSECT, METER, all_of(keep.cols))

# Isolate the community matrix (the 'diff' columns)
comm.mat <- plants |> dplyr::select(starts_with("diff."))

plants$METER <- as.factor(plants$METER)

# Run the dbRDA with Euclidean distance
model <- capscale(comm.mat ~ TREATMENT, 
                  data = plants, 
                  distance = "euclidean")

# Check Significance
anova(model, by = "terms", permutations = how(blocks = plants$BLOCK))
RsquareAdj(model)

disp.treat <- betadisper(vegdist(comm.mat, method="euclidean"), plants$TREATMENT)
anova(disp.treat)
plot(disp.treat)
