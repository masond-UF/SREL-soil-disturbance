## ── Clear and load ──────────────────────────────────────────────────
rm(list = ls())
library(glmmTMB)
library(lme4)
library(lmerTest)
library(DHARMa)
library(emmeans)
library(dplyr)

surv <- read.csv('Plant-surveys/Clean-data/3_Soil-disturb-func-plants.csv')

surv$STEMS <- rowSums(surv[,9:22])

library(fitdistrplus)
descdist(surv$STEMS, discrete = TRUE)

mod.pois <- glmmTMB(STEMS ~ TREATMENT * METER + (1|BLOCK) + (1|BLOCK:TRANSECT),
                  data = surv, family = poisson)
sim.mod.pois <- simulateResiduals(mod.pois)
plot(sim.mod.pois)
testDispersion(sim.mod.pois)
summary(mod.pois)

mod.nb1 <- glmmTMB(STEMS ~ TREATMENT * METER + (1|BLOCK) + (1|BLOCK:TRANSECT),
                  data = surv, family = 'nbinom1')
sim.mod.nb1 <- simulateResiduals(mod.nb1)
plot(sim.mod.nb1)
testDispersion(sim.mod.nb1)
summary(mod.nb1)

mod.nb2 <- glmmTMB(STEMS ~ TREATMENT * METER + (1|BLOCK), # Block:Transect uninformative
                  data = surv, family = 'nbinom2')
sim.mod.nb2 <- simulateResiduals(mod.nb2)
plot(sim.mod.nb2)
testDispersion(sim.mod.nb2)
summary(mod.nb2)
Anova(mod.nb2, type = "III")

emmeans(mod.nb2, ~ TREATMENT, type = "response")
pairs(emmeans(mod.nb2, ~ TREATMENT, type = "response"))

emm <- emmeans(mod.nb2, ~ TREATMENT | METER, type = "response",
               at = list(METER = c(0, 1, 2, 3)))
emm.df <- as.data.frame(emm)

# Visualize

library(lattice)
library(latticeExtra)

emm.df$TREATMENT <- factor(emm.df$TREATMENT, levels = c("REF", "CC", "OC"))

cols <- c(REF = "grey40", CC = "#2166AC", OC = "#B2182B")
pchs <- c(REF = 16, CC = 15, OC = 17)
offsets <- c(REF = 0, CC = -0.06, OC = 0.06)

my.panel <- function(x, y, subscripts, ...) {
  grp <- as.character(emm.df$TREATMENT[subscripts])
  lcl <- emm.df$asymp.LCL[subscripts]
  ucl <- emm.df$asymp.UCL[subscripts]
  
  for (trt in c("REF", "CC", "OC")) {
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

p <- xyplot(response ~ METER, data = emm.df, groups = TREATMENT,
       type = "n",
       xlab = "Distance from carcass (m)",
       ylab = "Total stems (estimated marginal mean ± 95% CI)",
       ylim = c(5, 35),
       xlim = c(-0.2, 3.2),
       scales = list(x = list(at = 0:3)),
       panel = my.panel,
       key = list(
         space = "top",
         columns = 3,
         points = list(
           col = c("grey40", "#2166AC", "#B2182B"),
           pch = c(16, 15, 17),
           cex = 1.3
         ),
         lines = list(
           col = c("grey40", "#2166AC", "#B2182B"),
           lwd = 2
         ),
         text = list(
           lab = c("Reference", "Caged carcass", "Open carcass"),
           cex = 1.1
         )
       )
)

print(p)  ## view it

png("Plant-surveys/Figures/Stem-count.png", width = 7, height = 5, units = "in", res = 300)
print(p)
dev.off()

saveRDS(list(plot = p, data = emm.df), "Plant-surveys/Output/p_stems.rds")
