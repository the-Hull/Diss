
# Create time series and decompose trends from observed data --------------


ts_decomp <- function(data, fgroup, cellcode, expno=NULL, cutoff=1, plot=T){

## Create Subset for Data

      # Keep data after cutoff
      tmp <- data[data$TimeStep>=cutoff, ]

      # Number of experiments
      numexp <- length(expno)


      # Condition for Subsetting
      cond.fgroup <- condition_builder("FGroup", "==", fgroup)

      cond.cellcode <- condition_builder("CellCode", "==", cellcode)

      cond <- parse(text = paste(cond.fgroup, cond.cellcode, sep = " & "))


      # Condition for expno
      if(!is.null(expno) & numexp==1){

            cond.expno <- condition_builder("ExpNo", "==", expno)

            cond <- parse(text = paste(cond.fgroup, cond.cellcode, cond.expno, sep = " & "))


      } else if(!is.null(expno) & numexp > 1){

            cond.expno <- condition_builder("ExpNo", "==", expno, "|")

            cond <- parse(text = paste(cond.fgroup,
                                       cond.cellcode,
                                       paste( "(", cond.expno, ")"),
                                       sep = " & "))
      } else if(is.null(expno)){

            numexp <- length(unique(data$ExpNo))

            expno <- unique(data$ExpNo)

            cond.expno <- condition_builder("ExpNo", "==", expno, "|")

            cond <- parse(text = paste(cond.fgroup,
                                       cond.cellcode,
                                       paste( "(", cond.expno, ")"),
                                       sep = " & "))
      }



      tmp1 <- subset(tmp, eval(cond))

## Create Time Series Data

      # Split Data Frame by experiment (list env)
      list.exp <- split(tmp1, tmp1$ExpNo)

      # Create time series for each experiment (lis env)
      list.ts <- lapply(list.exp, function(x)
                              ts(x$Median, frequency=12, start=c(cutoff %/% 12, cutoff %% 12))
                        )

      # Decompose each time series
      list.dec <- lapply(list.ts, function(x) decompose(x, type='multiplicative'))

      # Store observations and trends
      list.decObs <- lapply(list.dec, function(a) a$x)
      list.decTrend <- lapply(list.dec, function(a) a$trend)

      tmp.list <- list(Observed=list.decObs, Trend=list.decTrend)


      if(plot==T){
      ## Set Up Graphics env

            # Plot Limits
            y.max <- max(log10(unlist(list.decObs)+1))*1.005
            y.min <- min(log10(unlist(list.decObs)+1))*.995


            # Open GFX
            x11()

            # Set up layout for plot
            layout <- layout(mat = matrix(c(1,3,2),
                                          byrow = T),
                              heights = c(0.45, 0.1, 0.45))

            # Counter for colors

            par(mar=c(4,4.5,1.5,0.5))

            for(k in 1:length(tmp.list)){

                  i <- 1

                  lapply(tmp.list[[k]], function(x)


                                          if(length(expno)==1){

                                                plot(log10(x+1),
                                                     xlab = "Time (years)",
                                                     ylab = "log Biomass Density [kg/sqkm]",
                                                     ylim = c(y.min, y.max),
                                                     main = paste(names(tmp.list)[k],
                                                                  "Values for experiment",
                                                                  expno
                                                            )

                                                     )


                                          } else if(length(expno) > 1 | is.null(expno)){


                                                if(i==1){

                                                      plot(log10(x+1),
                                                           xlab = "Time (years)",
                                                           ylab = "log Biomass Density [kg/sqkm]",
                                                           ylim = c(y.min, y.max)
                                                           ,type='l'
                                                           ,lwd=1.4,
                                                           main = paste(names(tmp.list)[k],
                                                                        "Values"
      #                                                                   ,ifelse(numexp>1, "Experiments", "Experiment"),
      #                                                                   paste(expno, collapse=", ")
                                                                        ))
                                                      i <<- i + 1


                                                } else {

                                                      lines(log10(x+1),
                                                            col=i
                                                            ,lwd=1.4)


                                                      i <<- i + 1

                                                      if (i == length(expno)+1){
                                                            i <<- 1
                                                      }
                                                }

                                          }

                  )
            }

            # NOTPLOT for legend

            par(mar=rep(0,4))
            plot(1:1, type="n", xlab="", ylab="", axes=F)
            text(1,1, paste(fgroup, "in", cellcode), cex=1.5, pos=3)

            legend("bottom",
                   legend = paste("Exp.:", expno),
                   col = 1:numexp,
                   lty=1,
                   lwd=2,
                   horiz=T,
                   bty = "n",
                   cex = 1.3)

            return(list(sub=tmp1, dec=list.dec))

            # return(list(sub=tmp1, dec=list.dec))
      } else {return(list(sub=tmp1, dec=list.dec))}
}