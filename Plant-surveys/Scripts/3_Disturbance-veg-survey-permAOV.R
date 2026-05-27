## --------------- HEADER ------------------------------------------------------
## Script name: 2_Disturbance-veg-survey-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-08-04
## Date Last Modified: 2023-08-04
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script analyzes the soil disturbance plant 
## vegetation survey

## --------------- SET THE WORK SPACE ------------------------------------------

library(tidyverse)
library(mvabund)
library(vegan)

# Clear the decks
rm(list=ls())

# Bring in the data
d <- read.csv('Plant-surveys/Clean-data/3_Soil-disturb-func-plants.csv')

## --------------- PREPARE THE DATA --------------------------------------------
# Add zone for comparisons
d$ZONE <- ifelse(d$METER <= 1, "carcass", "far")
d$ZONE <- factor(d$ZONE, levels = c("carcass", "far"))

## identify species/group columns (everything that isn't metadata)
meta.cols <- c("DATE", "BLOCK", "PLOT", "TREATMENT", "EXCLUSION",
               "ADDITION", "TRANSECT", "METER", "ZONE")
spp.cols <- setdiff(names(d), meta.cols)

## sum stems within zone per plot
d.sum <- d |>
  group_by(BLOCK, PLOT, TREATMENT, ZONE) |>
  summarize(across(all_of(spp.cols), sum, na.rm = TRUE), .groups = "drop")

d.sum$BLOCK <- factor(d.sum$BLOCK)
d.sum$TREATMENT <- factor(d.sum$TREATMENT, levels = c("REF", "CC", "OC"))

## --------------- MANYGLMs ----------------------------------------------------

## Omnibus
d.sum$TREATMENT.ZONE <- interaction(d.sum$TREATMENT, d.sum$ZONE, drop = TRUE)
comm <- mvabund(d.sum[, 5:18])

par(mfrow = c(1, 1))
meanvar.plot(comm, main = "Mean-variance for functional groups")

d.sum$log.effort <- log(ifelse(d.sum$ZONE == "carcass", 5, 8))


fit.omni <- manyglm(comm ~ TREATMENT * ZONE + BLOCK,
                     data = d.sum, family = "negative.binomial",
                     offset = d.sum$log.effort)
aov.omni <- anova(fit.omni, p.uni = "unadjusted", test = "LR", nBoot = 999)
print(aov.omni)

## Pairwise
run.pairwise <- function(dat, comm.cols, TREATMENT.a, TREATMENT.b, zone.label) {
  idx <- dat$TREATMENT %in% c(TREATMENT.a, TREATMENT.b) & dat$ZONE == zone.label
  sub <- dat[idx, ]
  sub$TREATMENT <- droplevels(sub$TREATMENT)
  mv  <- mvabund(sub[, comm.cols])
  fit <- manyglm(mv ~ TREATMENT + BLOCK, data = sub,
                 family = "negative.binomial")
  aov <- anova(fit, p.uni = "unadjusted", test = "LR", nBoot = 999)
  return(aov)
}

grp.cols <- c("CUD", "DICH", "DITE", "ANDRO", "ASTER.1", "ASTER.3",
              "ERIG", "PIGR", "RUBUS", "PINE",
              "OTHER.GRASS", "OTHER.FORB", "OTHER.WOODY", "OTHER.UNKNOWN")

## ── Carcass zone (0–1 m) ────────────────────────────────────────────

aov.carcass.oc.ref <- run.pairwise(d.sum, grp.cols, "OC", "REF", "carcass")
print(aov.carcass.oc.ref)

aov.carcass.cc.ref <- run.pairwise(d.sum, grp.cols, "CC", "REF", "carcass")
print(aov.carcass.cc.ref)

aov.carcass.oc.cc <- run.pairwise(d.sum, grp.cols, "OC", "CC", "carcass")
print(aov.carcass.oc.cc)

## ── Far zone (2–3 m) ────────────────────────────────────────────────

aov.far.oc.ref <- run.pairwise(d.sum, grp.cols, "OC", "REF", "far")
print(aov.far.oc.ref)

aov.far.cc.ref <- run.pairwise(d.sum, grp.cols, "CC", "REF", "far")
print(aov.far.cc.ref)

aov.far.oc.cc <- run.pairwise(d.sum, grp.cols, "OC", "CC", "far")
print(aov.far.oc.cc)

## --------------- PERMANOVA ---------------------------------------------------

# Omnibus
comm <- d.sum[, grp.cols]
perm.omni <- adonis2(comm ~ TREATMENT * ZONE,
                      data = d.sum, method = "bray",
                      permutations = how(blocks = d.sum$BLOCK, nperm = 9999))
print(perm.omni)

run.pairwise.perm <- function(dat, spp.cols, trt.a, trt.b, zone.label, nperm = 9999) {
  idx <- dat$TREATMENT %in% c(trt.a, trt.b) & dat$ZONE == zone.label
  sub <- dat[idx, ]
  sub$TREATMENT <- droplevels(sub$TREATMENT)
  comm <- sub[, spp.cols]
  perm <- how(blocks = sub$BLOCK, nperm = nperm)
  out <- adonis2(comm ~ TREATMENT, data = sub,
                 method = "bray", permutations = perm)
  return(out)
}

## ── Carcass zone (0–1 m) ────────────────────────────────────────────

perm.carcass.oc.ref <- run.pairwise.perm(d.sum, grp.cols, "OC", "REF", "carcass")
print(perm.carcass.oc.ref)

perm.carcass.cc.ref <- run.pairwise.perm(d.sum, grp.cols, "CC", "REF", "carcass")
print(perm.carcass.cc.ref)

perm.carcass.oc.cc <- run.pairwise.perm(d.sum, grp.cols, "OC", "CC", "carcass")
print(perm.carcass.oc.cc)

## ── Far zone (2–3 m) ────────────────────────────────────────────────

perm.far.oc.ref <- run.pairwise.perm(d.sum, grp.cols, "OC", "REF", "far")
print(perm.far.oc.ref)

perm.far.cc.ref <- run.pairwise.perm(d.sum, grp.cols, "CC", "REF", "far")
print(perm.far.cc.ref)

perm.far.oc.cc <- run.pairwise.perm(d.sum, grp.cols, "OC", "CC", "far")
print(perm.far.oc.cc)

# Betadisper
run.betadisper <- function(dat, spp.cols, trt.a, trt.b, zone.label) {
  idx <- dat$TREATMENT %in% c(trt.a, trt.b) & dat$ZONE == zone.label
  sub <- dat[idx, ]
  sub$TREATMENT <- droplevels(sub$TREATMENT)
  comm <- sub[, spp.cols]
  dis  <- vegdist(comm, method = "bray")
  bd   <- betadisper(dis, sub$TREATMENT)
  test <- permutest(bd, pairwise = TRUE)
  return(list(bd = bd, test = test))
}

bd.list <- list(
  carcass.oc.ref = run.betadisper(d.sum, grp.cols, "OC", "REF", "carcass"),
  carcass.cc.ref = run.betadisper(d.sum, grp.cols, "CC", "REF", "carcass"),
  carcass.oc.cc  = run.betadisper(d.sum, grp.cols, "OC", "CC",  "carcass"),
  far.oc.ref     = run.betadisper(d.sum, grp.cols, "OC", "REF", "far"),
  far.cc.ref     = run.betadisper(d.sum, grp.cols, "CC", "REF", "far"),
  far.oc.cc      = run.betadisper(d.sum, grp.cols, "OC", "CC",  "far")
)

bd.results <- data.frame(
  zone         = rep(c("carcass", "far"), each = 3),
  comparison   = rep(c("OC.vs.REF", "CC.vs.REF", "OC.vs.CC"), 2),
  betadisper.p = sapply(bd.list, function(x) x$test$tab["Groups", "Pr(>F)"])
)

cat("\n══════════════════════════════════════\n")
cat("BETADISPER (dispersion homogeneity)\n")
cat("══════════════════════════════════════\n")
print(bd.results, digits = 3)

r

## ── SIMPER on the full zone-level data ──────────────────────────────

comm.full <- d.sum[, grp.cols]

## treatment contrasts (pooling across zones since interaction NS)
simp.trt <- simper(comm.full, d.sum$TREATMENT, permutations = 999)
summary(simp.trt)

## zone contrasts (pooling across treatments)
simp.zone <- simper(comm.full, d.sum$ZONE, permutations = 999)
summary(simp.zone)

## ── NMDS ────────────────────────────────────────────────────────────

ord <- metaMDS(comm.full, distance = "bray", k = 2, trymax = 100)
cat("Stress:", ord$stress, "\n")

cols <- c(REF = "grey40", CC = "blue", OC = "red")
pch.zone <- c(carcass = 16, far = 17)

par(mfrow = c(1, 1))
plot(ord, type = "n", main = "NMDS")
points(ord, display = "sites",
       col = cols[as.character(d.sum$TREATMENT)],
       pch = pch.zone[as.character(d.sum$ZONE)],
       cex = 1.3)

d.sum$TRT.ZONE <- interaction(d.sum$TREATMENT, d.sum$ZONE, drop = TRUE)

for (trt in c("CC", "OC")) {
  for (zone in c("carcass", "far")) {
    grp <- paste0(trt, ".", zone)
    lty <- ifelse(zone == "carcass", 1, 3)
    idx <- d.sum$TRT.ZONE == grp
    if (sum(idx) >= 3) {
      ordiellipse(ord, groups = d.sum$TRT.ZONE, show.groups = grp,
                  col = cols[trt], kind = "se", conf = 0.95,
                  lwd = 2, lty = lty)
    }
  }
}

## REF as a single ellipse across both zones
ordiellipse(ord, groups = d.sum$TREATMENT, show.groups = "REF",
            col = cols["REF"], kind = "se", conf = 0.95,
            lwd = 2, lty = 1)

legend("topright",
       legend = c("CC", "OC", "REF", "", "carcass", "far"),
       col    = c("blue", "red", "grey40", NA, "black", "black"),
       pch    = c(16, 16, 16, NA, 16, 17),
       lty    = c(NA, NA, NA, NA, 1, 3),
       lwd    = c(NA, NA, NA, NA, 2, 2),
       bty = "n")

## species vectors
plot(ord, type = "n", main = "NMDS - species")
orditorp(ord, display = "species", col = "black", cex = 0.8)
