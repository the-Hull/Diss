
# Plot Time Series of BioMass Density, for a set of functional groups and experiments -----

data_plot <- function(data, expno=NULL, fgroup=NULL, plot=T, logscale=F, mf=T, id=F, CI=F,...){


#       y.lab <- paste(ifelse(logscale, "log", ""),
#             ylab)


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

            # Log / Unlog data
            if(logscale==T){
                  tmp.data$Median <- log10(tmp.data$Median+1)
            } else {

            }

            # Number of chosen experiments
            numexp <- length(expno)

            # Set number of rows / columns
            plot.layout <- if (is.null(expno) & is.null(fgroup)){
                                    c(1,2)
                              } else {
                                    c(numexp, 2)
                              }




            # open graphics device
            # x11()

            # Graphics Layout
            if(mf==T){
                  par(mfrow=plot.layout)
                  par(mar=c(4,4,1,1))
            } else {

            }

            # Dataframe for color matching
#             cols <- data.frame(FGroup=c("autotroph",
#                                         "carnivore",
#                                         "herbivore",
#                                         "omnivore"),
#                                Color=c("chartreuse3",
#                                        "orangered4",
#                                        "seagreen4",
#                                        "darkgoldenrod3"),
#                                stringsAsFactors = F)

            cols <- data.frame(FGroup=c("autotroph",
                                        "carnivore",
                                        "herbivore",
                                        "omnivore"),
                               Color=c("#A7F62F",
                                       "#CC2614" ,
                                       "#44CA9F" ,
                                       "#FFA41A"),
                               stringsAsFactors = F)

            exp.labels <- c("C(end) + C(ect)s + C(ect)i",
                           "C(ect)i + C(ect)s",
                           "C(end) + C(ect)i" ,
                           "C(end) + C(ect)s",
                           "C(ect)i",
                           "C(ect)s",
                           "C(end)",
                           "C(-)"
                           )


            # Loop for each experiment
            for(i in 1:numexp){

                  # temporary df for storing individual experiment data
                  tmp.data4plot <- subset(tmp.data, ExpNo==expno[i])





                  # Corresponding groups and cells
                  groups <- unique(tmp.data4plot$FGroup)
                  ngroups <- length(unique(tmp.data4plot$FGroup))

                  cells <- unique(tmp.data4plot$CellCode)
                  ncells <- length(unique(tmp.data4plot$CellCode))


                  # Loop for plotting each cell individually
                  for(k in 1:ncells){
                        # print(paste(cells[k], "in Loop"))

                        # Log / Unlog data


                        # temporary df for storing individual cell data
                        tmp2.data4plot <- tmp.data4plot[tmp.data4plot$CellCode==cells[k],]


                        # Loop for plotting corresponding data (Plot overlayed with  lines)
                        for(j in 1:ngroups){


                              # set up for CI
                              if(CI==T){
                                    xx <- c(1:length(unique(tmp2.data4plot$TimeStep)),
                                            rev(1:length(unique(tmp2.data4plot$TimeStep))))

                                    y1 <- if(logscale==T){
                                                 log10(tmp2.data4plot$medianLCI[tmp2.data4plot$FGroup==groups[j]])
                                                } else {
                                                 tmp2.data4plot$medianLCI[tmp2.data4plot$FGroup==groups[j]]
                                                }

                                    y2 <- rev(
                                          if(logscale==T){
                                                log10(tmp2.data4plot$medianUCI[tmp2.data4plot$FGroup==groups[j]])
                                          } else {
                                                tmp2.data4plot$medianUCI[tmp2.data4plot$FGroup==groups[j]]
                                          }
                                    )

                                    yy <- c(y1,y2)
                              }
                              # Identify color according to group in loop
                              colmatch <- cols[which(cols$FGroup==groups[j]),2]

                              # Make sure first plotting operation opens new plot window
                              if(j==1){

                                    # Plotting limits
                                    lowerLim <- min(tmp.data$Median)-min(tmp.data$Median)*0.05
                                    upperLim <- max(tmp.data$Median)*1.001


                                    plot(tmp2.data4plot$Median[tmp2.data4plot$FGroup==groups[j]],
                                         # xlab="Time Step (months)",
                                         # ylab=y.lab,
                                         col=colmatch,
                                         xlim=c(0, length(unique(tmp2.data4plot$TimeStep))),
                                         ylim=c(lowerLim,
                                                upperLim),
                                         type="l",
                                         lty=1,
                                         ...)

                                    if(CI==T){

                                          polygon(xx, yy,
                                                  col=add_alpha(colmatch, alpha = 0.4),
                                                  border=NA)
                                    }

                                    # ID Stamp

                                    if(id==T){

                                    text(round(length(unique(tmp.data4plot$TimeStep))*0.5),
                                         lowerLim+max(tmp.data$Median)*ifelse(logscale, 0.01, 0.85),
                                         paste0("E: ",
                                               expno[i],
                                               "   ",
                                               exp.labels[expno[i]],
                                               "        (",
                                               cells[k],
                                               ifelse(cells[k]=="Cell0", ": aseasonal)", ": seasonal)")
                                               )
                                         )
                                    }
                              # Plot remaining groups
                              } else {
                                    lines(tmp2.data4plot$Median[tmp2.data4plot$FGroup==groups[j]],
                                          col=colmatch,...)


                                    if(CI==T){
                                          polygon(xx, yy,
                                            col=add_alpha(colmatch, alpha = 0.4),
                                            border=NA)
                                    }
                              }
                        }
                  }
            }

            return(tmp.data)

      } else {tmp.data}

}

