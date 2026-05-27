library(lattice)
library(grid)
rm(list = ls())

## shared variables all panel functions need
cols <- c(REF = "grey40", CC = "#2166AC", OC = "#B2182B",
          CAGED = "#2166AC", OPEN = "#B2182B")
pchs <- c(REF = 16, CC = 15, OC = 17, CAGED = 15, OPEN = 17)
offsets <- c(REF = 0, CC = -0.06, OC = 0.06,
             "CAGED.2020" = -0.09, "CAGED.2022" = -0.03,
             "OPEN.2020" = 0.03, "OPEN.2022" = 0.09)
ltys <- c("2020" = 3, "2022" = 1)

## load all objects
obj.soil  <- readRDS("Soil-disturbance/Output/p-soil.rds")
obj.stems <- readRDS("Plant-surveys/Output/p_stems.rds")
obj.rich  <- readRDS("Plant-surveys/Output/p_rich.rds")
obj.bc    <- readRDS("Plant-surveys/Output/p-beta.rds")

## strip x-axis from top three, strip legends from bottom three
p.soil.nox <- update(obj.soil$plot, xlab = NULL)

p.stems.nox <- update(obj.stems$plot, xlab = NULL)
p.stems.nox$legend <- NULL

p.rich.clean <- update(obj.rich$plot, xlab = NULL)
p.rich.clean$legend <- NULL

p.bc.nokey <- obj.bc$plot
p.bc.nokey$legend <- NULL

## preview
emm.df <- obj.soil$data
print(p.soil.nox, split = c(1, 1, 1, 4), more = TRUE)
grid.text("(a)", x = 0.07, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold"))

emm.df <- obj.stems$data
print(p.stems.nox, split = c(1, 2, 1, 4), more = TRUE)
grid.text("(b)", x = 0.07, y = 0.73, gp = gpar(fontsize = 14, fontface = "bold"))

emm.df <- obj.rich$data
print(p.rich.clean, split = c(1, 3, 1, 4), more = TRUE)
grid.text("(c)", x = 0.07, y = 0.48, gp = gpar(fontsize = 14, fontface = "bold"))

emm.df <- obj.bc$data
print(p.bc.nokey, split = c(1, 4, 1, 4), more = FALSE)
grid.text("(d)", x = 0.07, y = 0.23, gp = gpar(fontsize = 14, fontface = "bold"))

## save
png("Plant-surveys/Figures/Combined-fig.png",
    width = 7, height = 18, units = "in", res = 300)

emm.df <- obj.soil$data
print(p.soil.nox, split = c(1, 1, 1, 4), more = TRUE)
grid.text("(a)", x = 0.07, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold"))

emm.df <- obj.stems$data
print(p.stems.nox, split = c(1, 2, 1, 4), more = TRUE)
grid.text("(b)", x = 0.07, y = 0.73, gp = gpar(fontsize = 14, fontface = "bold"))

emm.df <- obj.rich$data
print(p.rich.clean, split = c(1, 3, 1, 4), more = TRUE)
grid.text("(c)", x = 0.07, y = 0.48, gp = gpar(fontsize = 14, fontface = "bold"))

emm.df <- obj.bc$data
print(p.bc.nokey, split = c(1, 4, 1, 4), more = FALSE)
grid.text("(d)", x = 0.07, y = 0.23, gp = gpar(fontsize = 14, fontface = "bold"))

dev.off()