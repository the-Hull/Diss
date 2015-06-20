
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
      
            plot.layout <- if (is.null(expno) & is.null(fgroup)){c(1,2)}

            plot.layout <- if (!is.null(expno) & is.null(fgroup)){c(length(expno), 2)}

            x11()
            par(mfrow=plot.layout)

      } else {tmp.data}

}

