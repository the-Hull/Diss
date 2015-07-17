x11()

par(mfrow=c(4,2))

      cols <- data.frame(FGroup=c("autotrophs",
                                  "carnivores",
                                  "herbivores",
                                  "omnivores"),
                         Color=c("olivedrab",
                                 "orangered4",
                                 "seagreen4",
                                 "darkgoldenrod3"),
                         stringsAsFactors = F)



      col.bmd <- cols[c(1,3,4,2), 2]
      col.den <- cols[c(3,4,2), 2]

# apply(t(bardat), MARGIN = 2, FUN = function(x) barplot(x, beside=T, col = cols))

barplot(t(bardat), col = cols, beside=T)

bardat <- TL_plot(statsDens, expno = 1:8, cell = "Cell0", logscale = F, overplot = F)
bardatBMD <- TL_plot(statsFull, expno = 1:8, cell = "Cell0", logscale = F, overplot = F)
x11()
par(mar=c(3,4,2,0.5))
par(mfrow=c(2,1))
barplot(t(bardatBMD), col = col.bmd, beside=T, xlab="Experiments", ylab="log Biomass Density [n/sqkm]")
barplot(t(bardat), col = cols.bmd, beside=T, xlab="Experiments", ylab="log Density [n/sqkm]")
x11()
par(mfrow=c(2,1))
par(mar=c(3,4,2,0.5))
barplot(t(bardatBMD[,2:4]/bardat), col=col.den, beside=T, ylab="Average Biomass / Ind [kg]")
barplot(log10(t(bardatBMD[,2:4]/bardat)), col=col.den, beside=T, ylab="log Biomass / Ind [kg]")




# PrintOut ----------------------------------------------------------------

pdf("./prints/biomass_dens.pdf", paper="a4r")
par(mar=c(3,4,2,0.5))
par(mfrow=c(2,1))
barplot(t(bardatBMD), col = col.bmd, beside=T, xlab="Experiments", ylab="log Biomass Density [n/sqkm]",
        legend.text = cols$FGroup, args.legend = list(x="topright", bty="n", col=col.bmd))
barplot(t(bardat),
        beside=T,
        xlab="Experiments",
        ylab="log Density [n/sqkm]",
        col=col.den
        )
dev.off()
x11()

pdf("./prints/biomass_ind.pdf", paper="a4r")
par(mfrow=c(2,1))
par(mar=c(3,4,2,0.5))
barplot(t(bardatBMD[,2:4]/bardat), col=col.den, beside=T, ylab="Average Biomass / Ind [kg]",
        legend.text = cols$FGroup[-1],
        args.legend = list(bty="n", x="topleft"))
barplot(log10(t(bardatBMD[,2:4]/bardat)), col=col.den, beside=T, ylab="log Biomass / Ind [kg]")
dev.off()

