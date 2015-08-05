### Graph for time series, expno 1
load("./output/statsAov.Rda")
require(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[12pt]{scrbook}")


xl <- "\\textbf{Time Step (months)}"
# xl <- "Time Step (months)"
# xl <- "test"
yl <- "\\textbf{$\\log_{10}$ Biomass Density} \\\ $[kg\\cdot km^{-2}]$"
# yl <- "test"
# yl <- "Biomass Density [kg/sqkm]"


# x11()

#
tikz("../WriteUp/Dissertation/res/fig/ts_expno2.tex",
     width = 8,
     height = 8,
     standAlone = T,
     timestamp = T)
#
# png("../presentation/fig/ts_expno1.png", width = 10,height = 8,units = "in", res=300,
#      bg="transparent")

par(mfrow=c(2,1))
par(mgp= c(2.5, 0.75, 0))
par(mar=c(4,4.75,1.5,3))

dat <- subset(statsAov, CellCode=="Cell0")
data_plot(dat,expno = 2,
          logscale = T,
          CI=T,
          mf = F,
          lwd=2.5,
          xlab=xl,
          ylab=yl)

mtext("\\textbf{Cell 0 - Aseasonal}", side=4, line=0.5, las=0)


par(mar=c(4.5,4.75,1,3))
dat2 <- subset(statsAov, CellCode=="Cell1")
data_plot(dat2,expno = 2,
          logscale = T,
          CI=T,
          mf =F,
          lwd=2.5,
          xlab=xl,
          ylab=yl)

mtext("\\textbf{Cell 1 - Seasonal}", side=4, line=0.5, las=0)


dev.off()