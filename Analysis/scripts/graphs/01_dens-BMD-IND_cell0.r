load("./output/bootMedian_BMD.rda")
load("./output/bootMedian_IND.rda")

# Tikz Set up
require(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[12pt]{scrbook}")



avlab <- "\\textbf{Av. Body Mass $[kg\\cdot n^{-1}]$}"
# avlab <- "test"
bmdlab <- "\\textbf{Biomass Density $[kg\\cdot km^{-2}]$}"
indlab <- "\\textbf{Abundance Density $[n\\cdot km^{-2}]$}"

# avlab <- "Av. Body Mass [kg/n]"
# bmdlab <- "Biomass Density [kg/sqkm]"
# indlab <- "Abundance Density [n/sqkm]"


abh <- -.9
ablwd <- 6

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


# x11() ## run with plot=T once
barIND <- data_barplot(dat = bootmedian_IND,
                       logscale = F,
                       cell = 0,
                       addauto = T, plot=F)
# x11()
# tikz("../WriteUp/Dissertation/res/fig/BMD_IND_0.tex",
#      width = 8,
#      height = 8,
#      standAlone = T,
#      timestamp = T)


# png("../presentation/fig/BMD_IND_0.png", width = 8,height = 8,units = "in", res=300,
#     bg="transparent")
# x11()
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
             border="gray10",
             ylab= bmdlab
             )
abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")

abline(h=abh,
       col="black",
       lwd=ablwd)

## Legend

legend("topright",
       legend=cols$FGroup,
       pch=21,
       col="gray60",
       pt.bg=cols$Color,
       pt.cex=1.5,
       x.intersp = 0.7,
       y.intersp = 1.3,
       xjust=1,
       inset=c(.045,0.06),
       bg="white",
       box.col="white",
       box.lwd=0
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
             border="gray10",
             ylab=indlab,
             xaxt="n"
             )
abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")
mtext("Experiments", font=2, side=1, line=1)

abline(h=abh,
       col="black",
       lwd=ablwd)

###
# dev.off()
###
#
# #  Avg Body mass plot -----------------------------------------------------
tikz("../WriteUp/Dissertation/res/fig/BMD_IND_0_avg.tex",
     width = 8,
     height = 4,
     standAlone = T,
     timestamp = T)

# png("../presentation/fig/BMD_IND_0_avg.png", height=4, width=8, res=300, units="in",
#     bg="transparent")


par(mfrow=c(1,1))

locs <- barBMD$loc
avgBM <- barBMD$data/barIND$data
avgBM[1, ] <- NA
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
       lwd=3)

abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")

legend("topleft",
       legend=cols$FGroup[-1],
       pch=21,
       col="gray60",
       pt.bg=cols$Color[-1],
       pt.cex=1.5,
       x.intersp = 0.7,
       y.intersp = 1.3,
       xjust=1,
       inset=c(.055,0.06),
       bg="white",
       box.col="white",
       box.lwd=0
       # ,title="Trophic Group"
       # ,title.adj= 0.5
)


mtext("Experiments", font=2, side=1, line=2.5)

dev.off()