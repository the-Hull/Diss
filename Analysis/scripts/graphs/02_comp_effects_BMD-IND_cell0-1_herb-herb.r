## Graph for composition differences

## Tikz Set up
options(tikzDocumentDeclaration = "\\documentclass[12pt]{scrbook}")
#
# avlab <- "\\textbf{Av. Body Mass $[kg\\cdot n^{-1}]$}"
# bmdlab <- "\\textbf{Herbivore Biomass Density $[kg\\cdot km^{-2}]$}"
# indlab <- "\\textbf{Herbivore Abundance Density $[n\\cdot km^{-2}]$}"
bmdlab <- "\\textbf{$\\log_{10}$ Herbivore Biomass Density \\\ $[kg\\cdot km^{-2}]$}"
indlab <- "\\textbf{$\\log_{10}$ Herbivore Abundance Density \\\ $[n\\cdot km^{-2}]$}"
#

#
# avlab <- "Av. Body Mass [kg/n]"
# bmdlab <- "Biomass Density [kg/sqkm]"
# indlab <- "Abundance Density [n/sqkm]"


# x11()
#
# tikz("../WriteUp/Dissertation/res/fig/comp_effects_herb-herb.tex",
#      width = 8,
#      height = 8,
#      standAlone = T,
#      timestamp = T)
#
# # png("../presentation/fig/comp_effects_herb-herb.png", width = 10,height = 8,units = "in", res=300,
#      bg="transparent")

par(mar=c(4.5,
          4.5,
          0.5,
          0.5))
# x11()
dat <- comp_boxplot(statsAov, statsDensAov,cutoff=1080,type = NULL,
                    resp = c("herbivore","herbivore"),logscale = T,
                    labels=c(bmdlab, indlab))
# dev.off()


