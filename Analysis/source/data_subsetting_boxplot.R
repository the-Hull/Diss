
# Plot Time Series of BioMass Density, for a set of functional groups and experiments -----

data_boxplot <- function(data, expno=NULL, fgroup=NULL, plot=T, logscale=F,
                         cutoff=NULL){


      ## Check if any subsetting is required
      ## -----------------------------------


      # No Subsetting Required
      if(is.null(expno) & is.null(fgroup)){

            tmp.data <- data


            # Subsetting for expno argument only
      } else if (!is.null(expno) & is.null(fgroup)) {

            # Set conditional for subset according to argument length
            if (length(expno) > 1){
                  condition <- parse(text = condition_builder("ExpNo", "==", expno, "|" ))
            } else {
                  condition <- parse(text = condition_builder("ExpNo", "==", expno))
            }

            tmp.data <- subset(data, eval(condition))



            # Subsetting for fgroup argument only
      } else if (!is.null(fgroup) & is.null(expno)) {

            # Set conditional for subset according to argument length
            if (length(fgroup) > 1){
                  condition <- parse(text = condition_builder("FGroup", "==", fgroup, "|" ))
            } else {
                  condition <- parse(text = condition_builder("FGroup", "==", fgroup))
            }

            tmp.data <- subset(data, eval(condition))


            # Subsetting when both arguments supplied
      } else if (!is.null(expno) & !is.null(fgroup)){

            # Create conditional for experiment number subset
            if (length(expno) > 1){
                  condition.exp <- parse(text = condition_builder("ExpNo", "==", expno, "|" ))
            } else {
                  condition.exp <- parse(text = condition_builder("ExpNo", "==", expno))
            }

            tmp1.data <- subset(data, eval(condition.exp))


            # Create conditional for fgroup subset
            if (length(fgroup) > 1){
                  condition.fgroup <- parse(text = condition_builder("FGroup", "==", fgroup, "|" ))
            } else {
                  condition.fgroup <- parse(text = condition_builder("FGroup", "==", fgroup))
            }

            tmp.data <- subset(tmp1.data, eval(condition.fgroup))

      }


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


            groups <- unique(tmp.data$FGroup)
            ngroups <- length(groups)

            cells <- unique(tmp.data$CellCode)
            ncells <- length(unique(tmp.data$CellCode))


            # Dataframe for color matching
            cols <- data.frame(FGroup=c("autotroph",
                                        "carnivore",
                                        "herbivore",
                                        "omnivore"),
                               Color=c("olivedrab",
                                       "orangered4",
                                       "seagreen4",
                                       "darkgoldenrod3"),
                               stringsAsFactors = F)


            cols.con <- parse(text = condition_builder("FGroup", "==", groups, "|"))
            cols.sub <- subset(cols, eval(cols.con))




            # colors for boxplot

            boxcols <- as.character(gl(ngroups, numexp, labels=cols.sub$Color))


            exp.labels <- c("C(end) + C(ect)s + C(ect)i",
                            "C(ect)i + C(ect)s",
                            "C(end) + C(ect)i" ,
                            "C(end) + C(ect)s",
                            "C(ect)i",
                            "C(ect)s",
                            "C(end)",
                            "C(-)")


            ylab <- ifelse(logscale,
                           "log Biomass Density [kg/sqkm]",
                           "Biomass Density [kg/sqkm]")


            labels <- paste0("E:",
                            as.character(gl(numexp,
                                            1,
                                            numexp*ngroups,
                                            labels = as.character(expno))),
                            " ",
                            as.character(gl(ngroups,
                                            numexp,
                                            labels = groups)))

            print(labels)

            # open graphics device
            x11()

            # Graphics Layout
            par(mfrow=c(2,1))
            par(mar=c(7,6,1,1))


            # Loop for plotting each cell individually
            for(k in 1:ncells){
                  tmp.data4plot <- subset(tmp.data, CellCode==cells[k])

                  boxloc <- boxplot(Median~ExpNo+FGroup,
                          data=tmp.data4plot,
                          ylab = ylab,
                          col = boxcols,
                          x.axt = "n",
                          xlab = "",
                          axes=F,
                          frame.plot=TRUE
                          )
                  abline(v = ((1:(ngroups-1))*numexp)+0.5,
                         lty=2,
                         col='darkgrey')
                  axis(side = 1, at = 1:(ngroups*numexp), labels = labels, las=2)
                  axis(side = 2)

            }



      } else {tmp.data}

}

