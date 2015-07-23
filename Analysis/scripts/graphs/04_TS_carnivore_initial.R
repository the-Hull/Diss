## TS Decompose plot

load("./output/statsAov.Rda")
load("./output/statsDensAov.Rda")
## Tikz Set up

# x11()
tikz("../WriteUp/Dissertation/res/fig/BMD_ts_initial.tex",
     width = 8,
     height = 9,
     standAlone = T,
     timestamp = T, pointsize=12)
options(tikzDocumentDeclaration = "\\documentclass[12pt]{scrbook}")
par(cex=2.5)
par(ps=20)

ts_decomp(data = statsDensAov,
          fgroup = "carnivore",
          cutoff = 1,
          expno = 1:8,
          cellcode = "Cell0",
          lwd=2.5,
          ylab="\\textbf{$\\log_{10}$ Carnivore Biomass Density} \\\ $[kg\\cdot km^{-2}]$",
          xlab="\\textbf{Time (years)}",
          xlim=c(0,5))

dev.off()
