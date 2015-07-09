
# Plot Time Series of BioMass Density, for a set of functional groups and experiments -----


### DEBUG: Axis labels / ticks for expno = 1:8 produces error (only 3 fgroups for E8)

ratio_boxplot <- function(data, expno, logscale=F,
                         cutoff=NULL, plot=T, ylab){


      ## Check if any subsetting is required
      ## -----------------------------------

            # Set conditional for subset according to argument length
            if (length(expno) > 1){
                  condition <- parse(text = condition_builder("ExpNo", "==", expno, "|" ))
            } else {
                  condition <- parse(text = condition_builder("ExpNo", "==", expno))
            }

            tmp.data <- subset(data, eval(condition))






      if(!is.null(cutoff)){
            tmp.data <- tmp.data[tmp.data$TimeStep>=cutoff, ]
      } else {}


      # Set up graphics device --------------------------------------------------

      if (plot == T){

            # Log / Unlog data
            if(logscale==T){
                  tmp.data$Median <- log10(tmp.data$Median+1)
            } else {

            }

            # Number of chosen experiments / fgroups / cells
            numexp <- length(expno)

            cells <- unique(tmp.data$CellCode)
            ncells <- length(unique(tmp.data$CellCode))


            # Dataframe for color matching
            cols <- data.frame(ExpNo = 1:7,
                      Col = c("darkgreen",
                              rep(add_alpha("darkgreen", alpha=0.5),3),
                              rep(add_alpha("darkgreen", alpha=0.1),3)),
                      stringsAsFactors = F)

            boxcols <- cols$Col[cols$ExpNo[expno]]




            # colors for boxplot

#             boxcols <- as.character(gl(ngroups, numexp, labels=cols.sub$Color))
#
#
#             exp.labels <- c("C(end) + C(ect)s + C(ect)i",
#                             "C(ect)i + C(ect)s",
#                             "C(end) + C(ect)i" ,
#                             "C(end) + C(ect)s",
#                             "C(ect)i",
#                             "C(ect)s",
#                             "C(end)",
#                             "C(-)")
#
#
#             ylab <- ifelse(logscale,
#                            "log Biomass Density [kg/sqkm]",
#                            "Biomass Density [kg/sqkm]")
#
#
#             labels <- paste0("E:",
#                              as.character(gl(numexp,
#                                              1,
#                                              numexp*ngroups,
#                                              labels = as.character(expno))),
#                              " ",
#                              as.character(gl(ngroups,
#                                              numexp,
#                                              labels = groups)))
#
            # print(labels)

            # open graphics device
            x11()

            # Graphics Layout
            par(mfrow=c(2,1))
            par(mar=c(4.5,5,1,.5))


            # Loop for plotting each cell individually
            for(k in 1:ncells){
                  tmp.data4plot <- subset(tmp.data, CellCode==cells[k])

                  boxloc <- boxplot(Median~ExpNo,
                                    data=tmp.data4plot,
                                    ylab = ylab,
                                    col = boxcols,
                                    x.axt = "n",
                                    xlab = "Experiment",
                                    axes=T,
                                    frame.plot=TRUE
                  )

                  if(cells[k]=="Cell0"){
                        legend("topleft",
                               title="Functional Diversity",
                               legend=c("9", "8", "7"),
                               col = unique(boxcols),
                               pch = 15,
                               pt.cex = 1.5,
                               bty="n",
                               y.intersp = 0.9
                               )
                  }

#                   if (ngroups > 1){
#                         abline(v = ((1:(ngroups-1))*numexp)+0.5,
#                                lty=2,
#                                col='darkgrey')
#                   } else {}
#
#                   axis(side = 1, at = 1:(ngroups*numexp), labels = labels, las=2)
#                   axis(side = 2)

            }

            return(tmp.data)

      } else {tmp.data}

}

