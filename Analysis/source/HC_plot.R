## Plot HC.Ratio fo reach experiment, timestep and cell

HC_plot <- function(dat, expno, cutoff=1){

      numexp <- length(expno)
      cells <- unique(dat$CellCode)

      ## Subset data:

      if(numexp == 1){

            cond.exp <- condition_builder("ExpNo",
                                    "==",
                                    expno)
      } else {

            cond.exp <- condition_builder("ExpNo",
                                          "==",
                                          expno,
                                          "|")
      }

      ### subset for exp
      dat.tmp <- subset(dat, eval(parse(text = cond.exp)))

      ### subset for cutoff
      dat.tmp <- subset(dat.tmp, TimeStep>=cutoff)

      # Plotting ----------------------------------------------------------------
#
#       ylim.ce0.min <- log10(min(dat.tmp$Median[dat.tmp$CellCode=="Cell0"])*.95)
#       ylim.ce0.max <- log10(max(dat.tmp$Median[dat.tmp$CellCode=="Cell0"])*.105)
#
#
#       ylim.ce1.min <- log10(min(dat.tmp$Median[dat.tmp$CellCode=="Cell1"])*.95)
#       ylim.ce1.max <- log10(max(dat.tmp$Median[dat.tmp$CellCode=="Cell1"])*.105)
#



      x11()
      par(mfrow=c(numexp,2))
      par(mar=c(4.5,5.5,0.5,0.5))


      for(ex in 1:numexp){

            for(ce in cells){

                  dat.plot <- subset(dat.tmp, ExpNo==expno[ex] & CellCode==ce)

                  tsteps <- cutoff:max(dat.tmp$TimeStep)


                  ## Adjust Colors
                  ts <- length(tsteps)
                  col <- rep("orangered4", ts)
                  change.col <- which(log10(dat.plot$Median)>0)
                  col[change.col] <- "darkgreen"
                  col.eq <- which(log10(dat.plot$Median)==0)
                  col[col.eq] <- "darkblue"

                  ## Time in years
                  x <-  seq(from=(cutoff %/% 12 + (cutoff %% 12)/12 ),
                            to=(max(dat.plot$TimeStep) %/% 12 + (max(dat.plot$TimeStep) %% 12)/12 ),
                            by= 1/12)


#                   if(ce=="Cell0"){
#                         ylims <- c(ylim.ce0.min,ylim.ce0.max)
#                   } else {
#                         ylims <- c(ylim.ce1.min,ylim.ce1.max)
#                   }


                  plot(x=x,
                           y=log10(dat.plot$Median),
                       col="gray80",
                       pch=21,
                       bg=col,
                       type="n",
                       ylim=c(min(log10(dat.plot$medianLCI*.95)),
                              max(log10(dat.plot$medianUCI*1.05))),
                       xlab="Time (years)",
                       ylab="Herbivore : Carnivore \nBiomass Density [kg/sqm]"
                       )

                  abline(h=0, lty=2, col="gray50")


                  xx <- c(x,
                          rev(x))

                  yy <- c(log10(dat.plot$medianLCI), rev(log10(dat.plot$medianUCI)))
                  # yy.r <- c(log10(dat.plot$Min), rev(log10(dat.plot$Max)))


                  # polygon(xx,yy.r, border = NA, col=add_alpha("gray30", 0.4))

                  polygon(xx,yy, border = NA, col=add_alpha("gray70", 0.7))

                  points(x=x,
                       log10(dat.plot$Median),
                       type='o',
                       col="gray80",
                       pch=21,
                       bg=col
                       # ylim=ylims
                  )



            }
      }

}


