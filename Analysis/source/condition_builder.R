# Constructs a string of conditions to parse into subsetting ------------



condition_builder <- function(group, operator, match, modif=NULL){

      condi <- paste0(paste0(group,operator), paste0("'",match,"'"))

      if(length(match)>1 & !is.null(modif)){

            paster <- function(condi){

                  if(length(condi) > 1){
                        paste(condi[1], paster(condi[-1]), sep=paste0(" ", modif," "))
                  } else {
                        paste(condi)
                  }
            }
            return(paster(condi))


      } else if (length(match)==1 & is.null(modif)) {

            return(condi)

      } else if (length(match)==1 & !is.null(modif)) {

            warning("Supplied a modifier to a singular condition. Please try again.")

      } else if (length(match)>1 & is.null(modif)){

            warning("Modifier required. Please try again.")

      }
}
