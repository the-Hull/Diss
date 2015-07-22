### Graph for time series, expno 1

xl <- "\\textbf{Time Step (months)}"
# xl <- "test"
yl <- "\\textbf{$\\log_{10}$ Biomass Density} \\\ $[kg\\cdot km^{-2}]$"
# yl <- "test"

# x11()

#
tikz("../WriteUp/Dissertation/res/fig/ts_expno1.tex",
     width = 8,
     height = 8,
     standAlone = T,
     timestamp = T)

par(mfrow=c(2,1))

par(mar=c(4,4.5,1.5,3))

dat <- subset(statsAov, CellCode=="Cell0")
data_plot(dat,expno = 1,
          logscale = T,
          CI=T,
          mf = F,
          lwd=2,
          xlab=xl,
          ylab=yl)

# mtext("\\textbf{Cell 0 - Aseasonal}", side=4, line=0.5, las=0)


par(mar=c(4.5,4.5,1,3))
dat2 <- subset(statsAov, CellCode=="Cell1")
data_plot(dat2,expno = 1,
          logscale = T,
          CI=T,
          mf =F,
          lwd=2,
          xlab=xl,
          ylab=yl)

# mtext("\\textbf{Cell 1 - Seasonal}", side=4, line=0.5, las=0)


dev.off()