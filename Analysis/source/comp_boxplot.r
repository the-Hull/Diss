## Plot boxplots of carnivore composition and cell for autotroph
## and herbivore biomass and abundance density, resp.

## data needed: statsAov / statsDensAov (Median + composition factor)

comp_boxplot <- function(dat,dat2=NULL, resp, cutoff, type=NULL, logscale=F){

      numfg <- length(unique(dat$FGroup))

      ## cond. subsetting

      if(is.null(dat2)){

            if(type=="bmd" && numfg==4){
                  dat.tmp <- subset(dat, FGroup==resp[1] & TimeStep>=cutoff)
            } else if(type=="ind" && numfg==3){
                  dat.tmp <- subset(dat, FGroup==resp[2] & TimeStep>=cutoff)
            } else {
                  warning("Check data.")
            }

            nplot <- 1

      } else if(!is.null(dat2)) {
            dat.tmp <- subset(dat, FGroup==resp[1] & TimeStep>=cutoff)
            dat2.tmp <- subset(dat2, FGroup==resp[2] & TimeStep>=cutoff)
            nplot <- 2

      }


      if(logscale==T){

            dat.tmp$Median <- log10(dat.tmp$Median)

            if(!is.null(dat2)){
                  dat2.tmp$Median <- log10(dat2.tmp$Median)}
               else{}
      } else {}



      if(nplot==2){
            plotlist <-  list(dat.tmp, dat2.tmp)
      } else {
            plotlist <-list(dat.tmp)
      }


      ## create boxplot

      x11()
      par(mfrow=c(nplot,1))
      par(tcl=-.25)
      par(mgp= c(2.5, 0.75, 0))

      if(nplot>1){
            par(mar=c(3,4,2,0.5))
      }

      outplot <- lapply(plotlist,
                        FUN=function(x){
                              boxplot(Median ~ comp+CellCode,
                                      dat=x
                                      ,plot=F
                                      ,axes=F
                                      ,xaxt=F)
                        })
      id <- unlist(
                  lapply(
                        lapply(
                              strsplit(
                                    substr(outplot[[1]][["names"]],1,3)
                                    , ""),
                                    as.numeric),
                        sum)
                  )+1

      names <- substr(outplot[[1]][["names"]],1,3)

      # pal <- RColorBrewer::brewer.pal(4, "YlOrRd")
      pal <- c("gray95", "#ffeda0", "darkorange2", "firebrick4")


      cols <- character(length(id))
      cols <- paste(pal[id])


      i <- 1
      ylabs <- c(paste0(ifelse(logscale==T,"log ",""),
                        "Biomass Density [kg/sqkm]"),
                 paste0(ifelse(logscale==T,"log ",""),
                        "Abundance Density [n/sqkm]")
                  )

      invisible(lapply(plotlist,
             FUN=function(x){
                   boxplot(Median ~ comp+CellCode,
                           dat=x,
                           col=cols,
                           names=names,
                           ylab=ylabs[i])


                   if(i==1){
                         legend("bottomleft",
                                bty="n",
                                pch=21,
                                pt.bg=rev(pal),
                                legend=c("3","2", "1", "none"),
                                col=1,
                                pt.cex=1.5,
                                title="Carnivorous\nGroups",
                                xjust = 0.5,
                                inset=c(0.35, .1),
                                y.intersp=0.8,
                                title.adj = 0.5,
                                yjust = 0
                         )



                  }

                   abline(v=8.5, lty=2, col="gray85")

                  mtext(text = c("Cell 0","Cell 1"),
                        side = 3,
                        line = .5,
                        at = c(4,13),
                        font = 2)

                   i<<-i+1

             }))

      text(x = 3, y=9e6, (" A | B | C"))
      text(x = 1.5, y=8e6,"A = ECT-i")
      text(x = 3, y=8e6, "B = ECT-s")
      text(x = 4.5, y=8e6, "C = END")
      rect(.75, 7.25e6, 5.25, 9.85e6, border=T, density=-1)









}