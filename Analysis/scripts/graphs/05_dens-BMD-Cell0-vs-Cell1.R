require(tikzDevice)

load("output/bootMedian_BMD.rda")

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


ylab.auto <- "\\textbf{Autotroph Biomass Density \\ $[kg\\cdot km^{-2}]$}"
ylab.herb <- "\\textbf{Herbivore Biomass Density \\ $[kg\\cdot km^{-2}]$}"
ylab.car <- "\\textbf{Carnivore Biomass Density \\ $[kg\\cdot km^{-2}]$}"

## extract data for Cell1 + Cell2
dat1 <- bootmedian_BMD$median[,,1]
dat2 <- bootmedian_BMD$median[,,2]

## Adjust Names for disting. after binding
dimnames(dat2)[[1]] <- paste0(dimnames(dat2)[[1]], "_Cell1")

#Bind and adjust rownames
datbar <- as.data.frame(rbind(dat1, dat2))
datbar$FGroup <- row.names(datbar)
datbar <- datbar[order(datbar$FGroup),]

auto.matbar <- as.matrix(datbar[grep("auto", datbar$FGroup), -9])
herb.matbar <- as.matrix(datbar[grep("herb", datbar$FGroup), -9])
car.matbar <- as.matrix(datbar[grep("car", datbar$FGroup), -9])





## Repeat for upper Conf
uci1 <- bootmedian_BMD$UCI[,,1]
uci2 <- bootmedian_BMD$UCI[,,2]

dimnames(uci2)[[1]] <- paste0(dimnames(uci2)[[1]], "_Cell1")

ucibar <- as.data.frame(rbind(uci1, uci2))
ucibar$FGroup <- row.names(ucibar)
ucibar <- ucibar[order(ucibar$FGroup),]
## Make sets for plotting
auto.ucibar <- as.matrix(ucibar[grep("auto", ucibar$FGroup), -9])
herb.ucibar <- as.matrix(ucibar[grep("herb", ucibar$FGroup), -9])
car.ucibar <- as.matrix(ucibar[grep("car", ucibar$FGroup), -9])



## Repeat for lower Conf

lci1 <- bootmedian_BMD$LCI[,,1]
lci2 <- bootmedian_BMD$LCI[,,2]

dimnames(lci2)[[1]] <- paste0(dimnames(lci2)[[1]], "_Cell1")

lcibar <- as.data.frame(rbind(lci1, lci2))
lcibar$FGroup <- row.names(lcibar)
lcibar <- lcibar[order(lcibar$FGroup),]

## Make sets for plotting
auto.lcibar <- as.matrix(lcibar[grep("auto", lcibar$FGroup), -9])
herb.lcibar <- as.matrix(lcibar[grep("herb", lcibar$FGroup), -9])
car.lcibar <- as.matrix(lcibar[grep("car", lcibar$FGroup), -9])





## Plotting
# ---- Auto
# x11()
options(tikzDocumentDeclaration = "\\documentclass[12pt]{scrbook}")

tikz("../WriteUp/Dissertation/res/fig/BMD_Cell1-2_AHC.tex",
     width = 8,
     height = 10,
     standAlone = T,
     timestamp = T)


par(mfrow=c(3,1))
par(mar=c(1,6,3.5,0.5))
par(cex=1)
par(cex.axis=0.8)
par(cex.lab=0.8)

pos.auto <- barplot(auto.matbar,
                    beside = T,
                    ylim = c(0, max(auto.matbar, na.rm=T)+85000),
                    ylab = ylab.auto,
                    col = c("#A7F62F", add_alpha("#A7F62F", 0.3)),
                    yaxp=c(0, 2e06,2),
                    xaxt="n"
                    )





midp.auto <- apply(pos.auto, MARGIN = 2, mean)
text(x = midp.auto,
     apply(auto.ucibar, MARGIN = 2, max) + 70000,
     labels = "***",
     cex = 1.5,
     xpd=T)

abline(h=0, lwd=2)
arrows(x0 = as.numeric(pos.auto),
       y0 = as.numeric(auto.matbar),
       x1 = as.numeric(pos.auto),
       y1 = as.numeric(auto.ucibar),
       angle=90,
       length = 0.05,
       col="grey10")


arrows(x0 = as.numeric(pos.auto),
       y0 = as.numeric(auto.matbar),
       x1 = as.numeric(pos.auto),
       y1 = as.numeric(auto.lcibar),
       angle=90,
       length = 0.05,
       col="grey10")

abline(v=seq(3.5, 21.5, by=3), col="gray60", lty=2, lwd=1.5)
# mtext(text = line1, side = 1,line = 2.5,outer = F,at = midp.auto)
# mtext(text = line2, side = 1,line = 3.5,outer = F,at = midp.auto)
# mtext(text = line3, side = 1,line = 4.5,outer = F,at = midp.auto)

legend("topright", legend = c("Aseasonal", "Seasonal"),
       col = c("#A7F62F", add_alpha("#A7F62F", 0.3)),
       pch = 16,
       bty="o",
       box.col = "white",
       bg="white",
       cex=.9)
#


# ## ---
#
# ## HERB-----
#
#
# # x11()
par(mar=c(1,6,1,0.5))

pos.herb <- barplot(herb.matbar,
                    beside = T,
                    ylim = c(0, max(herb.matbar, na.rm=T)+17500),
                    ylab = ylab.herb,
                    col = c("#44CA9F", add_alpha("#44CA9F", 0.3)),
                    yaxp=c(0, 3.5e05,2),
                    xaxt="n"
                    )



midp.herb <- apply(pos.herb, MARGIN = 2, mean)
text(x = midp.herb,
     apply(herb.matbar, MARGIN = 2, max) + 15000,
     labels = "***",
     cex = 1.5,
     xpd=T)

abline(h=0, lwd=2)
abline(v=seq(3.5, 21.5, by=3), col="gray60", lty=2, lwd=1.5)

arrows(x0 = as.numeric(pos.herb),
       y0 = as.numeric(herb.matbar),
       x1 = as.numeric(pos.herb),
       y1 = as.numeric(herb.ucibar),
       angle=90,
       length = 0.05,
       col="grey10")




arrows(x0 = as.numeric(pos.herb),
       y0 = as.numeric(herb.matbar),
       x1 = as.numeric(pos.herb),
       y1 = as.numeric(herb.lcibar),
       angle=90,
       length = 0.05,
       col="grey10")


legend("topright", legend = c("Aseasonal", "Seasonal"),
       col = c("#44CA9F", add_alpha("#44CA9F", 0.3)),
       pch = 16,
       bty="o",
       box.col = "white",
       bg="white",
       cex=.9)
#
#
# mtext(text = line1, side = 1,line = 2.5,outer = F,at = midp.herb)
# mtext(text = line2, side = 1,line = 3.5,outer = F,at = midp.herb)
# mtext(text = line3, side = 1,line = 4.5,outer = F,at = midp.herb)
#

# ------ CAR

# x11()
par(mar=c(4.5,6,1,0.5))

pos.car <- barplot(car.matbar,
                    beside = T,
                    ylim = c(0, max(car.matbar, na.rm=T)+15000),
                    ylab = ylab.car,
                    col = c("#CC2614", add_alpha("#CC2614", 0.3)),
                        yaxp=c(0, 1.5e05,2),
                   xaxt="n"
                    )



midp.car <- apply(pos.car, MARGIN = 2, mean)
text(x = midp.car,
     apply(car.ucibar, MARGIN = 2, max) + 10000,
     labels = "***",
     cex = 1.5,
     xpd=T)

abline(h=0, lwd=2)
abline(v=seq(3.5, 21.5, by=3), col="gray60", lty=2, lwd=1.5)


arrows(x0 = as.numeric(pos.car),
       y0 = as.numeric(car.matbar),
       x1 = as.numeric(pos.car),
       y1 = as.numeric(car.ucibar),
       angle=90,
       length = 0.05,
       col="grey10")



arrows(x0 = as.numeric(pos.car),
       y0 = as.numeric(car.matbar),
       x1 = as.numeric(pos.car),
       y1 = as.numeric(car.lcibar),
       angle=90,
       length = 0.05,
       col="grey10")


legend("topright", legend = c("Aseasonal", "Seasonal"),
       col = c("#CC2614", add_alpha("#CC2614", 0.3)),
       pch = 16,
       bty="o",
       box.col = "white",
       bg="white",
       cex=.9)

mtext(text = 1:8, side = 1,line = 1,outer = F,at = midp.car)

mtext(text = line1, side = 1,line = 2,outer = F,at = midp.car)
mtext(text = line2, side = 1,line = 3,outer = F,at = midp.car)
mtext(text = line3, side = 1,line = 4,outer = F,at = midp.car)

dev.off()