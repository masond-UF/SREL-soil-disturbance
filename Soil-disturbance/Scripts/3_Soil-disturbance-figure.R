rm(list = ls())
library(glmmTMB)
library(emmeans)
library(dplyr)
library(lattice)

soil <- read.csv('Soil-disturbance/Clean-data/Soil-disturb-figures')
soil$YEAR <- as.factor(format(as.Date(soil$DATE.RD), "%Y"))
soil$BLOCK <- factor(soil$BLOCK)
soil$EXCLUSION <- factor(soil$EXCLUSION, levels = c("CAGED", "OPEN"))

## ── Average across cardinal directions at each distance ─────────────
## Keep only cardinal axis points (DISTANCE = 0, 1, 2, 3)
soil.card <- soil[soil$DISTANCE %in% c(0, 1, 2, 3), ]

## ── Model ───────────────────────────────────────────────────────────
mod <- glmmTMB(cbind(SUCCESSES, FAILURES) ~ EXCLUSION * DISTANCE * YEAR + (1|BLOCK),
               data = soil.card, family = binomial)

summary(mod)
car::Anova(mod, type = "III")

## ── Emmeans at each distance x exclusion x year ─────────────────────
emm <- emmeans(mod, ~ EXCLUSION | DISTANCE + YEAR, type = "response",
               at = list(DISTANCE = c(0, 1, 2, 3)))
emm.df <- as.data.frame(emm)
pairs(emm)

## ── Lattice figure ──────────────────────────────────────────────────
emm.df$grp <- interaction(emm.df$EXCLUSION, emm.df$YEAR)

cols <- c(CAGED = "#2166AC", OPEN = "#B2182B")
pchs <- c(CAGED = 15, OPEN = 17)
ltys <- c("2020" = 3, "2022" = 1)
offsets <- c("CAGED.2020" = -0.09, "CAGED.2022" = -0.03,
             "OPEN.2020" = 0.03, "OPEN.2022" = 0.09)

my.panel <- function(x, y, subscripts, ...) {
  grp <- as.character(emm.df$grp[subscripts])
  lcl <- emm.df$asymp.LCL[subscripts]
  ucl <- emm.df$asymp.UCL[subscripts]
  exc <- as.character(emm.df$EXCLUSION[subscripts])
  yr  <- as.character(emm.df$YEAR[subscripts])
  
  for (g in c("CAGED.2020", "OPEN.2020", "CAGED.2022", "OPEN.2022")) {
    idx <- grp == g
    if (sum(idx) == 0) next
    this.exc <- exc[idx][1]
    this.yr  <- yr[idx][1]
    xx <- x[idx] + offsets[g]
    yy <- y[idx]
    
    panel.lines(xx, yy, col = cols[this.exc], lwd = 2, lty = ltys[this.yr])
    panel.points(xx, yy, col = cols[this.exc], pch = pchs[this.exc], cex = 1.4)
    panel.arrows(xx, lcl[idx], xx, ucl[idx],
                 col = cols[this.exc], length = 0.03,
                 angle = 90, code = 3, lwd = 1.5)
  }
}

trellis.par.set(list(
  axis.text     = list(cex = 1.1, font = 1),
  par.xlab.text = list(cex = 1.2, font = 1),
  par.ylab.text = list(cex = 1.2, font = 1)
))

p.soil <- xyplot(prob ~ DISTANCE, data = emm.df, groups = grp,
       type = "n",
       xlab = "Distance from carcass (m)",
       ylab = "Soil disturbance proportion (± 95% CI)",
       ylim = c(0, max(emm.df$asymp.UCL) + 0.05),
       xlim = c(-0.2, 3.2),
       scales = list(x = list(at = 0:3)),
       panel = my.panel,
    	 key = list(
         space = "top",
         columns = 2,
         lines = list(
           col = c("#2166AC", "#B2182B", "#2166AC", "#B2182B"),
           lwd = 2,
           lty = c(3, 3, 1, 1)
         ),
         points = list(
           col = c("#2166AC", "#B2182B", "#2166AC", "#B2182B"),
           pch = c(15, 17, 15, 17),
           cex = 1.3
         ),
         text = list(
           lab = c("Caged 2020", "Open 2020", "Caged 2022", "Open 2022"),
           cex = 1.1
         )
       )
)

print(p.soil)

png("Soil-disturbance/Figures/Soil-disturb.png", width = 7, height = 5, units = "in", res = 300)
print(p.soil)
dev.off()

saveRDS(list(plot = p.soil, data = emm.df), "Soil-disturbance/Output/p-soil.rds")

