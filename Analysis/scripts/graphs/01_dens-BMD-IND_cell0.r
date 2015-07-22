avlab <- "\\textbf{Av. Body Mass $[kg\\cdot n^{-1}]$}"
bmdlab <- "\\textbf{Biomass Density $[$kg\\cdot km^{-2}]$}"
indlab <- "\\textbf{Abundance Density $[$n\\cdot km^{-2}]$}"

# Graph:: Barplot - BMD / IND - Cell 0 ------------------------------------
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


barIND <- data_barplot(dat = bootmedian_IND,
                       logscale = F,
                       cell = 0,
                       addauto = T, plot=F)
x11()
par(mfrow=c(2,1))


## Biomass Density Plot
par(mar=c(2,
          4.5,
          0.5,
          0.5))
barBMD <- data_barplot(dat = bootmedian_BMD,
             logscale = F,
             cell = 0,
             ## Graph Options
             border="gray40",
             ylab= bmdlab
             )
abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")

## Legend

legend("topright",
       legend=cols$FGroup,
       pch=21,
       col="gray60",
       pt.bg=cols$Color,
       pt.cex=1.5,
       x.intersp = 0.4,
       y.intersp = 1.3,
       bty="n",
       xjust=1,
       inset=c(.045,0.06)
       # ,title="Trophic Group"
       # ,title.adj= 0.5
       )



# Abundance Density -------------------------------------------------------


par(mar=c(2.5,
          4.5,
          0.5,
          0.5))
data_barplot(dat = bootmedian_IND,
             logscale = F,
             cell = 0,
             addauto = T,
             ## Graph Options
             border="gray40",
             ylab=indlab,
             xaxt="n"
             )
abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")
mtext("Experiments", font=2, side=1, line=1)



#  Avg Body mass plot -----------------------------------------------------

locs <- barBMD$loc
avgBM <- barBMD$data/barIND$data
avgBM[1, ] <- 0

x11()
par(mar=c(4.5,
          4.5,
          0.5,
          0.5))



barplot(avgBM,
        beside=T,
        col=cols$Color,
        ylim=c(0,8),
        ylab=avlab)

abline(h=0,
       col="black",
       lwd=1.5)

abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")
mtext("Experiments", font=2, side=1, line=2.5)
