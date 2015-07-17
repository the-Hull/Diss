TL_plot <- function(dat, expno, cell, logscale=T, overplot=F, cutoff=1080){


      numexp <- length(expno)


#       cols <- data.frame(FGroup=c("autotrophs",
#                                   "carnivores",
#                                   "herbivores",
#                                   "omnivores"),
#                          Color=c("olivedrab",
#                                  "orangered4",
#                                  "seagreen4",
#                                  "darkgoldenrod3"),
#                          stringsAsFactors = F)




      tmp <- dat[dat$TimeSte>=1080, ]

      tmp <- tapply(tmp$Median,
             list(tmp$ExpNo, tmp$FGroup, tmp$CellCode),
             mean)

      n <- ncol(tmp)

      labels <- c("Autotroph",
               "Herbivore",
               "Omnivore",
               "Carnivore")

      if(n==3){
            tmp <- tmp[expno,c(2,3,1),cell]
            labels <- labels[-1]
      } else {
            tmp <- tmp[expno,c(1,3,4,2),cell]
      }



      if(logscale==T){

            tmp <- log10(tmp)

      }

      ylim=c(0.95*min(tmp, na.rm=T), 1.05*max(tmp, na.rm=T))

      x11()

      if(overplot==F){


            if(numexp>1){
                  par(mfrow=c(ceiling(numexp/2),2))
                  par(mar=c(4.5,4,0.5,0.5))
            } else {
                  par(mfrow=c(1,1))
                  par(mar=c(3.5,4,1,0.5))

            }

            for(i in 1:numexp){

                  if(numexp==1){
                        plot(1:n,
                             tmp[, i],
                             type="n",
                             xaxt="n",
                             xlab="Trophic Group",
                             ylab="Biomass Density [kg/sqkm]",
                             ylim=ylim)

                        lines(1:n, tmp[, i], col=1:numexp, type="o", lwd=1.5)

                  } else {
                        plot(1:n,
                             tmp[i, ],
                             type="n",
                             xaxt="n",
                             xlab="Trophic Group",
                             ylab="Biomass Density [kg/sqkm]",
                             ylim=ylim)


                        lines(1:n, tmp[i, ], col="darkblue", lwd=1.5, type="o")
                  }
                  axis(side = 1, at = 1:n, labels=labels)
            }


      }  else {


            for(i in 1:numexp){

                  if(i==1){
                        plot(1:n, tmp[i, ],
                             type="n",
                             xaxt="n",
                             xlab="Trophic Group",
                             ylab="Biomass Density [kg/sqkm]",
                             ylim=ylim)

                        lines(1:n, tmp[i,], col=i, lwd=1.5, type="o")

                  } else if (expno[i]==8){
                        lines(1:3, tmp[i,1:3], col=i, lwd=1.5, type="o")

                  } else {
                        lines(1:n, tmp[i,], col=i, lwd=1.5, type="o")
                        }

            }

            legend("top",
                   horiz=T,
                   legend=expno[1:numexp],
                   col=1:numexp,
                   lwd=1.5,
                   pch=16,
                   bty="n",
                   seg.len=1.5,
                   x.intersp=0.8,
                   title=paste(cell, "- Experiment:")
                   )

            axis(side = 1, at = 1:n, labels=labels)
            }

      tmp
}