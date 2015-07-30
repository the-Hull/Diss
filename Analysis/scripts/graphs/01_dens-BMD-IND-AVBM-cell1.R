load("./output/bootMedian_BMD.rda")
load("./output/bootMedian_IND.rda")
load("./output/bootMedian_Avbm.Rda")

# Tikz Set up
require(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[12pt]{scrbook}")




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




avlab <- "\\textbf{Av. Body Mass $[kg\\cdot n^{-1}]$}"
# avlab <- "test"
bmdlab <- "\\textbf{Biomass Density $[kg\\cdot km^{-2}]$}"
indlab <- "\\textbf{Abundance Density $[n\\cdot km^{-2}]$}"

# avlab <- "Av. Body Mass [kg/n]"
# bmdlab <- "Biomass Density [kg/sqkm]"
# indlab <- "Abundance Density [n/sqkm]"


abh <- -.9
ablwd <- 2

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
                       cell = 1,
                       addauto = T, plot=T)
# x11()
tikz("../WriteUp/Dissertation/res/fig/BMD_IND_AVBM_1.tex",
     width = 8,
     height = 10,
     standAlone = T,
     timestamp = T)
par(cex=1.3)
par(cex.axis=1.3)
par(cex.lab=1.5)


# png("../presentation/fig/BMD_IND_0.png", width = 8,height = 8,units = "in", res=300,
#     bg="transparent")
# x11()
par(mfrow=c(3,1))


## Biomass Density Plot
par(mar=c(1,
          4.5,
          0.5,
          0.5))
barBMD <- data_barplot(dat = bootmedian_BMD,
                       logscale = F,
                       cell = 1,
                       ## Graph Options
                       border="gray10",
                       ylab= bmdlab,
                       xaxt="n",
                       yaxp=c(0, 10^6,2)
                  )
abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")

abline(h=abh,
       col="black",
       lwd=ablwd)

text(x = barBMD$loc[2,],
     y = barBMD$data[2,]+100000,
     labels =  c("a", "ab", "ab", "b", "a", "b", "b", "ab"),
     cex=1.3)

## Legend




# Abundance Density -------------------------------------------------------


par(mar=c(1,
          4.5,
          0.5,
          0.5))
ind.loc <- data_barplot(dat = bootmedian_IND,
                        logscale = F,
                        cell = 1,
                        addauto = T,
                        ## Graph Options
                        border="gray10",
                        ylab=indlab,
                        xaxt="n",
                        yaxp=c(0, 10^6,2)
                        )
abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")
# mtext("Experiments", font=2, side=1, line=1)

abline(h=abh,
       col="black",
       lwd=ablwd)

legend("topleft",
       legend=cols$FGroup,
       pch=21,
       col="gray60",
       pt.bg=cols$Color,
       pt.cex=1.5,
       x.intersp = 0.7,
       y.intersp = 1.3,
       xjust=1,
       inset=c(.01,0.07),
       bg="white",
       box.col="white",
       box.lwd=0,
       cex=1.5,
       xpd=T,
       ncol=2
       # ,title="Trophic Group"
       # ,title.adj= 0.5
)

text(x = ind.loc$loc[2,],
     y = ind.loc$data[2,]+150000,
     labels =  c("a", "a", "b", "ab", "b", "ab", "c", "c"),
     cex=1.3,
     xpd=T)

###
# dev.off()
###
#
# #  Avg Body mass plot -----------------------------------------------------
# tikz("../WriteUp/Dissertation/res/fig/BMD_IND_0_avg.tex",
#      width = 8,
#      height = 4,
#      standAlone = T,
#      timestamp = T)

# png("../presentation/fig/BMD_IND_0_avg.png", height=4, width=8, res=300, units="in",
#     bg="transparent")


# par(mfrow=c(1,1))

# par(mar=c(5,2.5,0.5,0.5))


par(mar=c(6.5,
          4.5,
          0.5,
          0.5))


locs <- data_barplot(dat = bootMedian_AVBM,
                     logscale = F,
                     cell = 1,
                     addauto = T,
                     ## Graph Options
                     border="gray10",
                     ylab=avlab
)
abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")
# mtext("Experiments", font=2, side=1, line=1)

abline(h=abh,
       col="black",
       lwd=ablwd)



abline(h=0,
       col="black",
       lwd=3)

abline(v=seq(5.5,35.5,5), lty=2, lwd=1.5, col="gray60")

# legend("topleft",
#        legend=cols$FGroup[-1],
#        pch=21,
#        col="gray60",
#        pt.bg=cols$Color[-1],
#        pt.cex=1.5,
#        x.intersp = 0.7,
#        y.intersp = 1.3,
#        xjust=1,
#        inset=c(.01,0.06),
#        bg="white",
#        box.col="white",
#        box.lwd=0,
#        cex=1.5
#        # ,title="Trophic Group"
#        # ,title.adj= 0.5
# )

text(x = locs$loc[2,],
     y = locs$data[2,]+.16,
     labels =  c("a", "a", "b", "ab", "b", "b", "c", "c"),
     cex=1.3,
     xpd=T)



midp <- apply(locs$loc, MARGIN = 2, mean)

mtext(text = line1, side = 1,line = 2.5,outer = F,at = midp, xpd = T)
mtext(text = line2, side = 1,line = 3.5,outer = F,at = midp, xpd = T)
mtext(text = line3, side = 1,line = 4.5,outer = F,at = midp, xpd = T)

# mtext("Experiments", font=2, side=1, line=2.5)

dev.off()