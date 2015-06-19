
# Plot Time Series of BioMass Density, for a set of functional gro --------

data_plot <- function(data, expno=NULL, fgroup=NULL){

      if(is.null(expno) & is.null(fgroup)){

            tmp.data <- data

      } else if (!is.null(expno) & is.null(fgroup)) {

            if (expno > 1){
                  condition <- condition_builder("ExpNo", "==", expno, "|" )
            } else {
                  condition <- condition_builder("ExpNo", "==", expno)
            }

            tmp.data <- subset(data, condition)
      }



}