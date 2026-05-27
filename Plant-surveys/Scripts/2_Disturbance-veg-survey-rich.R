
library(lme4)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(dplyr)
library(fitdistrplus)
library(car)

rm(list = ls())

rich <- read.csv('Plant-surveys/Clean-data/2_Richness.csv')

rich$BLOCK     <- factor(rich$BLOCK)
rich$TREATMENT <- factor(rich$TREATMENT, levels = c("REF", "CC", "OC"))
# rich$METER     <- factor(rich$METER)
rich$TRANSECT  <- factor(rich$TRANSECT)

library(fitdistrplus)
descdist(rich$RICHNESS, discrete = TRUE)

mod <- glmmTMB(RICHNESS ~ TREATMENT * METER + (1|BLOCK), # Block:Transect drop
                  data = rich, family = 'compois')

summary(mod)
Anova(mod, type = 3)

sim <- simulateResiduals(mod)
plot(sim, main = "Poisson")
testDispersion(sim)

emmeans(mod, ~ TREATMENT, type = "response")
emtrends(mod, ~ TREATMENT, var = "METER", type = "response")

## ── Model estimates at each meter ───────────────────────────────────
emm <- emmeans(mod, ~ TREATMENT | METER, type = "response",
               at = list(METER = c(0, 1, 2, 3)))
emm.df <- as.data.frame(emm)
emm.df$TREATMENT <- factor(emm.df$TREATMENT, levels = c("REF", "CC", "OC"))

## ── Lattice figure ──────────────────────────────────────────────────
library(lattice)

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
       ylab = "Richness (estimated marginal mean ± 95% CI)",
       ylim = c(0, max(emm.df$asymp.UCL) + 1),
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

print(p)

png("Plant-surveys/Figures/Richness.png", width = 7, height = 5, units = "in", res = 300)
print(p)
dev.off()

saveRDS(list(plot = p, data = emm.df), "Plant-surveys/Output/p_rich.rds")
