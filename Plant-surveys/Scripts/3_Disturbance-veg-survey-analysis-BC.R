## ── Clear and load ──────────────────────────────────────────────────
rm(list = ls())
library(vegan)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(dplyr)

d <- read.csv('Plant-surveys/Clean-data/3_Soil-disturb-func-plants.csv')

## ── Identify species columns ────────────────────────────────────────
meta.cols <- c("DATE", "BLOCK", "PLOT", "TREATMENT", "EXCLUSION",
               "ADDITION", "TRANSECT", "METER")
spp.cols <- setdiff(names(d), meta.cols)

d$TOTAL <- rowSums(d[, spp.cols])
d[d$TOTAL == 0, c("BLOCK", "TREATMENT", "TRANSECT", "METER", "TOTAL")]
d$TOTAL <- NULL

## ── 1. Build reference composite per block ──────────────────────────

ref <- d[d$TREATMENT == "REF", ]

ref.composite <- ref |>
  group_by(BLOCK) |>
  summarize(across(all_of(spp.cols), sum, na.rm = TRUE), .groups = "drop")

		## ── 2. Compute Bray-Curtis from each OC/CC subsample to its block ref

carcass <- d[d$TREATMENT %in% c("OC", "CC"), ]

carcass$BC.TO.REF <- NA

for (i in 1:nrow(carcass)) {
  blk <- carcass$BLOCK[i]
  ref.row <- ref.composite[ref.composite$BLOCK == blk, spp.cols]
  sub.row <- carcass[i, spp.cols]
  ## if subsample is all zeros, BC is 1 (maximally dissimilar)
  if (sum(sub.row) == 0) {
    carcass$BC.TO.REF[i] <- 1
  } else {
    pair <- rbind(as.numeric(sub.row), as.numeric(ref.row))
    carcass$BC.TO.REF[i] <- vegdist(pair, method = "bray")[1]
  }
}

## ── 3. Check the response ───────────────────────────────────────────

hist(carcass$BC.TO.REF, breaks = 30, main = "Bray-Curtis to block reference")
summary(carcass$BC.TO.REF)

## beta regression can't handle exact 0 or 1 — squeeze if needed
eps <- 1e-6
carcass$BC.TO.REF <- pmin(pmax(carcass$BC.TO.REF, eps), 1 - eps)

## ── 4. Set up factors ───────────────────────────────────────────────

carcass$BLOCK     <- factor(carcass$BLOCK)
carcass$TREATMENT <- factor(carcass$TREATMENT, levels = c("CC", "OC"))
carcass$METER     <- factor(carcass$METER)
carcass$TRANSECT  <- factor(carcass$TRANSECT)

## ── 5. Fit beta regression with random effects ──────────────────────

fit <- glmmTMB(BC.TO.REF ~ TREATMENT * METER + (1|BLOCK) + (1|BLOCK:TRANSECT),
               data = carcass,
               family = beta_family(link = "logit"))

summary(fit)

## ── 6. Diagnostics ──────────────────────────────────────────────────

sim <- simulateResiduals(fit)
plot(sim)

## ── 7. Marginal means ───────────────────────────────────────────────

emm <- emmeans(fit, ~ TREATMENT | METER, type = "response")
print(emm)
pairs(emm)

## ── 8. Visualize ────────────────────────────────────────────────────

emm.df <- as.data.frame(emm)

par(mfrow = c(1, 1))
plot(as.numeric(as.character(emm.df$METER)),
     emm.df$response,
     col  = ifelse(emm.df$TREATMENT == "OC", "red", "blue"),
     pch  = 16, cex = 1.5,
     xlab = "Meter", ylab = "Bray-Curtis to reference",
     main = "Dissimilarity to block reference",
     ylim = c(0, 1))

for (trt in c("CC", "OC")) {
  sub <- emm.df[emm.df$TREATMENT == trt, ]
  col <- ifelse(trt == "OC", "red", "blue")
  lines(as.numeric(as.character(sub$METER)), sub$response, col = col, lwd = 2)
  arrows(as.numeric(as.character(sub$METER)), sub$asymp.LCL,
         as.numeric(as.character(sub$METER)), sub$asymp.UCL,
         angle = 90, code = 3, length = 0.05, col = col)
}

legend("topright", legend = c("OC", "CC"),
       col = c("red", "blue"), pch = 16, lwd = 2, bty = "n")

## ── 1. Build reference composite per block ──────────────────────────
## Pool all REF subsamples within a block into one community vector

ref <- d[d$TREATMENT == "REF", ]

ref.composite <- ref |>
  group_by(BLOCK) |>
  summarize(across(all_of(spp.cols), sum, na.rm = TRUE), .groups = "drop")

## ── 2. Compute Bray-Curtis from each OC/CC subsample to its block ref

carcass <- d[d$TREATMENT %in% c("OC", "CC"), ]

carcass$BC.TO.REF <- NA

for (i in 1:nrow(carcass)) {
  blk <- carcass$BLOCK[i]
  ref.row <- ref.composite[ref.composite$BLOCK == blk, spp.cols]
  sub.row <- carcass[i, spp.cols]
  ## bind into 2-row matrix, compute bray-curtis
  pair <- rbind(as.numeric(sub.row), as.numeric(ref.row))
  carcass$BC.TO.REF[i] <- vegdist(pair, method = "bray")[1]
}

## ── 3. Check the response ───────────────────────────────────────────

hist(carcass$BC.TO.REF, breaks = 30, main = "Bray-Curtis to block reference")
summary(carcass$BC.TO.REF)

## beta regression can't handle exact 0 or 1 — squeeze if needed
eps <- 1e-6
carcass$BC.TO.REF <- pmin(pmax(carcass$BC.TO.REF, eps), 1 - eps)

## ── 4. Set up factors ───────────────────────────────────────────────

carcass$BLOCK     <- factor(carcass$BLOCK)
carcass$TREATMENT <- factor(carcass$TREATMENT, levels = c("CC", "OC"))
carcass$METER     <- factor(carcass$METER)  # 0, 1, 2, 3 as factor
carcass$TRANSECT  <- factor(carcass$TRANSECT)

## ── 5. Fit beta regression with random effects ──────────────────────

fit <- glmmTMB(BC.TO.REF ~ TREATMENT * METER + (1|BLOCK) + (1|BLOCK:TRANSECT),
               data = carcass,
               family = beta_family(link = "logit"))

summary(fit)

## ── 6. Quick diagnostics ────────────────────────────────────────────

library(DHARMa)
sim <- simulateResiduals(fit)
plot(sim)

## ── 7. Marginal means if interaction is interesting ─────────────────

library(emmeans)
emm <- emmeans(fit, ~ TREATMENT | METER, type = "response", adjust = 'none')
print(emm)
pairs(emm)

## ── 8. Visualize ────────────────────────────────────────────────────

emm.df <- as.data.frame(emm)

par(mfrow = c(1, 1))
plot(as.numeric(as.character(emm.df$METER)),
     emm.df$response,
     col  = ifelse(emm.df$TREATMENT == "OC", "red", "blue"),
     pch  = 16, cex = 1.5,
     xlab = "Meter", ylab = "Bray-Curtis to reference",
     main = "Dissimilarity to block reference",
     ylim = c(0, 1))

for (trt in c("CC", "OC")) {
  sub <- emm.df[emm.df$TREATMENT == trt, ]
  col <- ifelse(trt == "OC", "red", "blue")
  lines(as.numeric(as.character(sub$METER)), sub$response, col = col, lwd = 2)
  arrows(as.numeric(as.character(sub$METER)), sub$asymp.LCL,
         as.numeric(as.character(sub$METER)), sub$asymp.UCL,
         angle = 90, code = 3, length = 0.05, col = col)
}

legend("topright", legend = c("OC", "CC"),
       col = c("red", "blue"), pch = 16, lwd = 2, bty = "n")


## ── SIMPER at meter 0 ───────────────────────────────────────────────

center <- d[d$METER == 0, ]
center.comm <- center[, spp.cols]
center.trt  <- center$TREATMENT

## all three pairwise comparisons
simp <- simper(center.comm, center.trt, permutations = 999)
summary(simp)

## ── Raw mean stems at center by treatment and species ───────────────

center.long <- center |>
  tidyr::pivot_longer(cols = all_of(spp.cols),
                      names_to = "SPECIES", values_to = "STEMS") |>
  group_by(TREATMENT, SPECIES) |>
  summarize(mean.stems = mean(STEMS), .groups = "drop")

## keep only species with mean > 0.5 in at least one treatment
keep <- center.long |>
  group_by(SPECIES) |>
  filter(max(mean.stems) > 0.5) |>
  pull(SPECIES) |>
  unique()

center.plot <- center.long[center.long$SPECIES %in% keep, ]
center.plot$TREATMENT <- factor(center.plot$TREATMENT, levels = c("REF", "CC", "OC"))

## grouped barplot
library(ggplot2)

ggplot(center.plot, aes(x = SPECIES, y = mean.stems, fill = TREATMENT)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(REF = "grey40", CC = "blue", OC = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Mean stems at center (0 m)",
       title = "Species composition at carcass center by treatment")

## ── Same thing but for all distances ────────────────────────────────

all.long <- d |>
  tidyr::pivot_longer(cols = all_of(spp.cols),
                      names_to = "SPECIES", values_to = "STEMS") |>
  group_by(TREATMENT, METER, SPECIES) |>
  summarize(mean.stems = mean(STEMS), .groups = "drop")

## facet by distance, only species that matter
all.plot <- all.long[all.long$SPECIES %in% keep, ]
all.plot$TREATMENT <- factor(all.plot$TREATMENT, levels = c("REF", "CC", "OC"))

ggplot(all.plot, aes(x = SPECIES, y = mean.stems, fill = TREATMENT)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(REF = "grey40", CC = "blue", OC = "red")) +
  facet_wrap(~ METER, labeller = labeller(METER = function(x) paste0(x, " m"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Mean stems",
       title = "Species composition by treatment and distance")

## ── 8. Lattice figure ───────────────────────────────────────────────
library(lattice)

emm.df <- as.data.frame(emm)
emm.df$TREATMENT <- factor(emm.df$TREATMENT, levels = c("CC", "OC"))
emm.df$METER.num <- as.numeric(as.character(emm.df$METER))

cols <- c(CC = "#2166AC", OC = "#B2182B")
pchs <- c(CC = 15, OC = 17)
offsets <- c(CC = -0.04, OC = 0.04)

my.panel <- function(x, y, subscripts, ...) {
  grp <- as.character(emm.df$TREATMENT[subscripts])
  lcl <- emm.df$asymp.LCL[subscripts]
  ucl <- emm.df$asymp.UCL[subscripts]
  
  for (trt in c("CC", "OC")) {
    idx <- grp == trt
    if (sum(idx) == 0) next
    xx <- x[idx] + offsets[trt]
    yy <- y[idx]
    
    panel.lines(xx, yy, col = cols[trt], lwd = 2)
    panel.points(xx, yy, col = cols[trt], pch = pchs[trt], cex = 1.4)
    panel.arrows(xx, lcl[idx], xx, ucl[idx],
                 col = cols[trt], length = 0.03,
                 angle = 90, code = 3, lwd = 1.5)
  }
}

trellis.par.set(list(
  axis.text     = list(cex = 1.1, font = 1),
  par.xlab.text = list(cex = 1.2, font = 1),
  par.ylab.text = list(cex = 1.2, font = 1)
))

p.bc <- xyplot(response ~ METER.num, data = emm.df, groups = TREATMENT,
       type = "n",
       xlab = "Distance from carcass (m)",
       ylab = "Bray-Curtis dissimilarity to reference (± 95% CI)",
       ylim = c(0.75, 1),
       xlim = c(-0.2, 3.2),
       scales = list(x = list(at = 0:3)),
       panel = my.panel,
       key = list(
         space = "top",
         columns = 2,
         points = list(
           col = c("#2166AC", "#B2182B"),
           pch = c(15, 17),
           cex = 1.3
         ),
         lines = list(
           col = c("#2166AC", "#B2182B"),
           lwd = 2
         ),
         text = list(
           lab = c("Caged carcass", "Open carcass"),
           cex = 1.1
         )
       )
)

print(p.bc)

png("Plant-surveys/Figures/Beta-div.png", width = 7, height = 5, units = "in", res = 300)
print(p.bc)
dev.off()

saveRDS(list(plot = p.bc, data = emm.df), "Plant-surveys/Output/p-beta.rds")
