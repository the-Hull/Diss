require(dplyr)
require(tikzDevice)
load("./output/HARatios_medianCI.Rda")
load("./output/OARatios_medianCI.Rda")
load("./output/CARatios_medianCI.Rda")
emp <- read.csv("./report_data/empirical_ratios.csv",
                header = T,
                stringsAsFactors = F)

emp$LHerA <- log10(emp$HARatio)

serengeti <- median(emp$LHerA[emp$Name=="Serengeti"])
ser.min <- min(emp$LHerA[emp$Name=="Serengeti"])
ser.max <- max(emp$LHerA[emp$Name=="Serengeti"])

andrews <- median(emp$LHerA[emp$Name=="Andrews Experimental Forest, Oregon"])
and.min <- min(emp$LHerA[emp$Name=="Andrews Experimental Forest, Oregon"])
and.max <- max(emp$LHerA[emp$Name=="Andrews Experimental Forest, Oregon"])


barrow <- median(emp$LHerA[emp$Name=="Barrow, Alaska"])

cedar <- median(emp$LHerA[emp$Name=="Cedar Creek Natural History Area, Minnesota"])

devon <- median(emp$LHerA[emp$Name=="Devon Island, Nunavut"])

#
# HA.summary$FGroup <- "Herb-Auto"
# HerbA_bootMedian <- boot_resamp(HA.summary, 1080, 10^4)
#
#
#
# CA.summary$FGroup <- "Car-Auto"
# CarA_bootMedian <- boot_resamp(CA.summary, 1080, 10^4)
#
#
# OA.summary$FGroup <- "Omn-Auto"
# OmnA_bootMedian <- boot_resamp(OA.summary, 1080, 10^4)

# bootMedian_ratios <- new.env()
# bootMedian_ratios$HA <- HerbA_bootMedian
# bootMedian_ratios$CA <- CarA_bootMedian
# bootMedian_ratios$OA <- OmnA_bootMedian
# save(bootMedian_ratios, file="output/bootMedian_RatiosBMD.Rda")
load("./output/bootMedian_RatiosBMD.Rda")
HerbA_bootMedian <-  bootMedian_ratios$HA
CarA_bootMedian <- bootMedian_ratios$CA
OmnA_bootMedian <- bootMedian_ratios$OA
##### PLOT ---------------
# Def:

alpha <- 1
ptcex <- 2
a.length <- 0.1

require(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[12pt]{scrbook}")



tikz("../WriteUp/Dissertation/res/fig/HetAut-Ratios.tex",
     width = 8,
     height = 10,
     standAlone = T,
     timestamp = T)
par(mfrow=c(2,1))
par(mar=c(1,5,1,0.5))


ylab <-  "$\\log_{10}$ \\textbf{Heterotroph : Autotroph} \\ \\textbf{Biomass Ratios}"
cols <- data.frame(FGroup=c("autotrophs",
                            "carnivores",
                            "herbivores",
                            "omnivores"),
                   Color=c("#A7F62F",
                           "#CC2614" ,
                           "#44CA9F" ,
                           "#FFA41A"),
                   stringsAsFactors = F)

cols <- cols[c(1,3,4,2), ]

## Plot Herb + Auto ratios with adjust x + ylims
plot(log10(HerbA_bootMedian$median[,1]),
     xlim = c(1,8),
     ylim = c(-2.6,0),
     pch = 16,
     col = add_alpha("#44CA9F", alpha),
     cex = ptcex,
     xaxt = "n",
     xlab = "",
     ylab = ylab
     )





points(seq(1.33, 7.33, 1),
       log10(CarA_bootMedian$median[,1]),
       pch = 16,
       col = add_alpha("#CC2614", alpha),
       cex = ptcex
      )

points(seq(1.66, 7.66, 1),
       log10(OmnA_bootMedian$median[,1]),
       pch = 16,
       col = add_alpha("#FFA41A", alpha),
       cex = ptcex
)


abline(v = seq(1.82, 6.82,1),
       lty = 2,
       lwd = 1.5,
       col = "gray60")


axis(side = 1, at = seq(1.33, 7.33, 1), labels = 1:7)

### CI BARS

arrows(x0 = 1:7,y0 = log10(HerbA_bootMedian$median[,1]),
       y1 = log10(HerbA_bootMedian$UCI[,1]),
       lwd = 1.5,
       col = add_alpha("#44CA9F", alpha),
       angle = 90, length = a.length
)

arrows(x0 = 1:7,y0 = log10(HerbA_bootMedian$median[,1]),
       y1 = log10(HerbA_bootMedian$LCI[,1]),
       lwd = 1.5,
       col = add_alpha("#44CA9F", alpha),
       angle = 90, length = a.length
)

#### CARNIVORE
arrows(x0 = 1.33:7.33,y0 = log10(CarA_bootMedian$median[,1]),
       y1 = log10(CarA_bootMedian$UCI[,1]),
       lwd = 1.5,
       col = add_alpha("#CC2614", alpha),
       angle = 90, length = a.length
)

arrows(x0 = 1.33:7.33,y0 = log10(CarA_bootMedian$median[,1]),
       y1 = log10(CarA_bootMedian$LCI[,1]),
       lwd = 1.5,
       col = add_alpha("#CC2614", alpha),
       angle = 90, length = a.length
)


## OMNIVORE
arrows(x0 = 1.66:7.66,y0 = log10(OmnA_bootMedian$median[,1]),
       y1 = log10(OmnA_bootMedian$UCI[,1]),
       lwd = 1.5,
       col = add_alpha("#FFA41A", alpha),
       angle = 90, length = a.length
)

arrows(x0 = 1.66:7.66,y0 = log10(OmnA_bootMedian$median[,1]),
       y1 = log10(OmnA_bootMedian$LCI[,1]),
       lwd = 1.5,
       col = add_alpha("#FFA41A", alpha),
       angle = 90, length = a.length
)

### Empirical

arrows(x0 = 1, y0 = serengeti,
       y1 = ser.max,
       angle = 90, length = a.length)


arrows(x0 = 1, y0 = serengeti,
       y1 = ser.min,
       angle = 90, length = a.length)

text(x = 1, y = serengeti, label = "Serengeti,\nTanzania", pos = 4)

points(1,
       serengeti,
       pch = 21,
       col = "black",
       bg = "#44CA9F",
       cex = ptcex,
       lwd = 2
)


# points(1,
#        cedar,
#        pch = 21,
#        col = "black",
#        bg = "#44CA9F",
#        cex = ptcex,
#        lwd = 2
# )


legend("topleft",
       legend=cols$FGroup[-1],
       pch=21,
       col="gray60",
       pt.bg=cols$Color[-1],
       pt.cex=1.5,
       x.intersp = 0.7,
       y.intersp = 1.3,
       xjust=1,
       inset=c(.01,0.07),
       bg="white",
       box.col="white",
       box.lwd=0,
       cex=1,
       xpd=T,
       ncol=1
       # ,title="Trophic Group"
       # ,title.adj= 0.5
)
legend("topright", "A", bty="n", cex=1.5)



### Cell 1
par(mar=c(4.5,5,1,0.5))


## Plot Herb + Auto ratios with adjust x + ylims
plot(log10(HerbA_bootMedian$median[,2]),
     xlim = c(1,8),
     ylim = c(-5,0),
     pch = 16,
     col = add_alpha("#44CA9F", alpha),
     cex = ptcex,
     xaxt = "n",
     xlab = "",
     ylab = ylab
)





points(seq(1.33, 7.33, 1),
       log10(CarA_bootMedian$median[,2]),
       pch = 16,
       col = add_alpha("#CC2614", alpha),
       cex = ptcex
)

points(seq(1.66, 7.66, 1),
       log10(OmnA_bootMedian$median[,2]),
       pch = 16,
       col = add_alpha("#FFA41A", alpha),
       cex = ptcex
)


abline(v = seq(1.82, 6.82,1),
       lty = 2,
       lwd = 1.5,
       col = "gray60")


# axis(side = 1, at = seq(1.33, 7.33, 1), labels = 1:7)

### CI BARS

arrows(x0 = 1:7,y0 = log10(HerbA_bootMedian$median[,2]),
       y1 = log10(HerbA_bootMedian$UCI[,2]),
       lwd = 1.5,
       col = add_alpha("#44CA9F", alpha),
       angle = 90, length = a.length
)

arrows(x0 = 1:7,y0 = log10(HerbA_bootMedian$median[,2]),
       y1 = log10(HerbA_bootMedian$LCI[,2]),
       lwd = 1.5,
       col = add_alpha("#44CA9F", alpha),
       angle = 90, length = a.length
)

#### CARNIVORE
arrows(x0 = 1.33:7.33,y0 = log10(CarA_bootMedian$median[,2]),
       y1 = log10(CarA_bootMedian$UCI[,2]),
       lwd = 1.5,
       col = add_alpha("#CC2614", alpha),
       angle = 90, length = a.length
)

arrows(x0 = 1.33:7.33,y0 = log10(CarA_bootMedian$median[,2]),
       y1 = log10(CarA_bootMedian$LCI[,2]),
       lwd = 1.5,
       col = add_alpha("#CC2614", alpha),
       angle = 90, length = a.length
)


## OMNIVORE
arrows(x0 = 1.66:7.66,y0 = log10(OmnA_bootMedian$median[,2]),
       y1 = log10(OmnA_bootMedian$UCI[,2]),
       lwd = 1.5,
       col = add_alpha("#FFA41A", alpha),
       angle = 90, length = a.length
)

arrows(x0 = 1.66:7.66,y0 = log10(OmnA_bootMedian$median[,2]),
       y1 = log10(OmnA_bootMedian$LCI[,2]),
       lwd = 1.5,
       col = add_alpha("#FFA41A", alpha),
       angle = 90, length = a.length
)




### Empirical

points(1,
       barrow,
       pch = 21,
       col = "black",
       bg = "#44CA9F",
       cex = ptcex,
       lwd = 2
)

text(x = 1.1, y = barrow-0.2, label = "Alaska,\nUSA", pos = 1)



####
points(2,
       devon,
       pch = 21,
       col = "black",
       bg = "#44CA9F",
       cex = ptcex,
       lwd = 2
)

text(x = 2, y = devon, label = "Alaska,\nUSA", pos = 4)



points(3,
       cedar,
       pch = 21,
       col = "black",
       bg = "#44CA9F",
       cex = ptcex,
       lwd = 2
)

text(x = 3.5, y = cedar*1.2, label = "Minnesota,\nUSA", pos = 1)





text(x = 4.35, y = andrews, label = "Oregon,\nUSA", pos = 3, offset=1)





arrows(x0 = 4, y0 = andrews,
       y1 = and.max,
       angle = 90, length = a.length)


arrows(x0 = 4, y0 = andrews,
       y1 = and.min,
       angle = 90, length = a.length)

points(4,
       andrews,
       pch = 21,
       col = "black",
       bg = "#44CA9F",
       cex = ptcex,
       lwd = 2
)




line1 <- c("\\textbf{$Ect_i$}",
           "\\textbf{$Ect_i$}",
           "\\textbf{$Ect_i$}",
           "\\textbf{$Ect_s$}",
           "\\textbf{$Ect_i$}",
           "\\textbf{$Ect_s$}",
           "\\textbf{$End_i$}",
           "\\textbf{-}")


line2 <- c("\\textbf{$Ect_s$}",
           "\\textbf{$Ect_s$}",
           "\\textbf{$End_i$}",
           "\\textbf{$End_i$}",
           "",
           "",
           "",
           "")
line3 <- c("\\textbf{$End_i$}",
           rep("", 7))

# ## Experiment Labels
mtext(side = 1, at = seq(1.33, 7.33, 1), text = line1[-8], line = 1)
mtext(side = 1, at = seq(1.33, 7.33, 1), text = line2[-8], line = 2)
mtext(side = 1, at = seq(1.33, 7.33, 1), text = line3[-8], line = 3)

legend("topright", "B", bty="n", cex=1.5)


## Legend


dev.off()
## Empirical