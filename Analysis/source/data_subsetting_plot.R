
# Plot Time Series of BioMass Density, for a set of functional gro --------

data_plot <- function(data, expno=NULL, fgroup=NULL, plot=T){


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

      # Set up graphics device --------------------------------------------------

      if (plot == T){

            plot.layout <- if (is.null(expno) & is.null(fgroup)){c(1,1)}

            plot.layout <- if (!is.null(expno) & is.null(fgroup)){c(length(expno), 1)}



            numexp <- length(expno)

            x11()

            par(mfrow=plot.layout)
            par(mar=c(4,4,1,1))
            cols <- c("red", "blue", "brown")

            for(i in 1:numexp){

                  tmp.data4plot <- subset(tmp.data, ExpNo==expno[i])
                  groups <- unique(tmp.data4plot$FGroup)
                  ngroups <- length(unique(tmp.data4plot$FGroup))

                  for(j in 1:ngroups){

                        if(j==1){
                              plot(tmp.data4plot$Median[tmp.data4plot$FGroup==groups[j]],
                                   xlab="Time Step (months)",
                                   ylab="Biomass Density [kg/sqkm]",
                                   col="darkgreen",
                                   xlim=c(100, 2402),
                                   ylim=c(0, 3000000),
                                   type="l",
                                   lty=1)

                              abline(v=1201, lty=2, col="darkgrey")
                        } else {
                              lines(tmp.data4plot$Median[tmp.data4plot$FGroup==groups[j]],
                                    col=cols[j])
                        }
                  }
            }



      } else {tmp.data}

}

