## Identify underlying distribution of each measure (Biosmass Dens) per time step

distribution_plot <- function(dat, stats, fgroup, expno=NULL, cellcode, cutoff=1){

      ## Set up
      # numfg <- length(fgroup)
      numexp <- ifelse(!is.null(expno), length(expno), 8)


      ## Check if fgroup = carnivore and expno == 8
      if(fgroup=="carnivore" & 8 %in% expno){
            warning("No carnivores present in experiment 8.")
      } else if(fgroup=="carnivore" & is.null(expno)){
            numexp <- 7
      }




      ## Create subsetting conditions
      cond.exp <- paste0("(",
                         ifelse(numexp > 1,
                                     condition_builder("ExpNo",
                                                "==",
                                                expno,
                                                "|"),
                                     condition_builder("ExpNo",
                                                       "==",
                                                       expno)),
                         ")")


#       cond.fg <- ifelse(numfg>1,
#                         condition_builder("FGroup",
#                                           "==",
#                                           fgroup,
#                                           "|"),
#                         condition_builder("FGroup",
#                                           "==",
#                                           fgroup))


      cond.fg <- condition_builder("FGroup",
                                          "==",
                                          fgroup)

      cond.cell <- condition_builder("CellCode",
                                       "==",
                                       cellcode)



      cond <- parse(text = paste(cond.exp, cond.fg, cond.cell, sep = " & "))
      print(cond)
      # Create subet
      tmp <- subset(dat,eval(cond))
      tmp <- tmp[tmp$TimeStep>=cutoff,]

      tsteps <- unique(tmp$TimeStep)
      numsteps <- length(unique(tmp$TimeStep))

      tmp.join <- left_join(tmp[,-c(2)], stats[, c(1:4,6)])
      tmp.join$adj <- log10(tmp.join$MassDens+1)-log10(tmp.join$Mean)
      print(head(tmp.join$adj))
      # Split each individual time step per experiment

      tmp.exp <- split(tmp.join, tmp.join$ExpNo, drop = T)



      x11()
      par(mfrow=c(ceiling(numexp/2),2))
      # loop over list to plot (lines) density dist. of timestep

      i <- 1
      lapply(tmp.exp, function(x){

            lmin <- min(x$adj)*1.5
            lmax <- max(x$adj)*1.5
            # ymax <- 4
                  for(ts in 1:numsteps){


                        tmp.mdens <- x[x$TimeStep==tsteps[ts], ]
#                         print(head(tmp.mdens$adj))
#                         print(length(tmp.mdens$adj))
                        ymax <- max(density(tmp.mdens$adj)$y)*1.3

                        if(ts==1){
                        plot(density(tmp.mdens$adj),
                             type='n',
                             xlim=c(lmin, lmax),
                             ylim=c(0, ymax),
                             main=paste0("Experiment ", expno[i], " (", fgroup, ")")
                             )
                        lines(density(tmp.mdens$adj), col=add_alpha("gray60", 0.05))

                        } else {
                        lines(density(tmp.mdens$adj), col=add_alpha("gray60", 0.5))
                        }
                  }
            i <<- i+1

      })



}