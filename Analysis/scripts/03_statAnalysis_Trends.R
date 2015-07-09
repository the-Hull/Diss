## First exploratory data analysis

### DEBUG: Add CI as polygons / ribbons

#  Identify ideal window for cutting off data  ---------------------------------

ts_trendplotID(50,95)


# Test if system dynamics follow normal distribution (for each t) ------------------



## Creates new env to store all o
plotdata <- new.env()

fg <- unique(expdataFull$FGroup)
expn <- unique(expdataFull$ExpNo)
cells <- unique(expdataFull$CellCode)

plotdata$Cell1 <- new.env()
plotdata$Cell0 <- new.env()

for(c in cells){
      for(e in expn){
            for(f in fg){


                  # assign respective subsets to objects
                  if(c=="Cell0"){
                        assign(paste0("exp",e,".",f, ".", c),
                               subset(expdataFull, FGroup==f & ExpNo==e & CellCode==c),
                               plotdata$Cell0)
                  } else {
                        assign(paste0("exp",e,".",f, ".", c),
                               subset(expdataFull, FGroup==f & ExpNo==e & CellCode==c),
                               plotdata$Cell1)
                  }


            }
      }
}


## Create graph space
x11()
layout(mat = matrix(c(1:4, rep(5,2)),
                    nrow=2),
       width=c(0.4,0.4,0.2)
)
## plot al experiments for each FGroup

for(ce in cells){
      for(f in 1:length(fg)){
            names <- ls(pattern = fg[f], name = plotdata[[ce]])

            for(n in 1:(length(names)-1)){

                  dat <- get(names[n], envir  = plotdata[[ce]])


                  stats <- summary_stats(dat)

                  ts <- length(unique(dat$TimeStep))



                  ## Plot if n == 1
                  if(n==1){
                        plot(stats$Median, ylim=c(min(stats$Min), max(stats$Max)))

                        xx <- c(stats$Min, rev(stats$Min))
                        yy <- c(stats$Max, rev(stats$Max))
                        polygon(xx, yy, col=add_alpha(2, alpha=0.8))
                        lines(1:nrow(stats), stats$Median, col=1, lwd=1.5)
                  } else {

                        lines(1:nrow(stats), stats$Median, col=1, lwd=1.5)
                        xx <- c(stats$Min, rev(stats$Min))
                        yy <- c(stats$Max, rev(stats$Max))
                        polygon(xx, yy, col="red")
                        lines(1:nrow(stats), stats$Median, col=1, lwd=1.5)

                  }


                  if(f==length(fg) & n==(length(names)-1)){
                              plot(1:1, type='n', axes=F, xaxt='n', yaxt='n', xlab="", ylab="")

                  }

#                   for(t in 1:ts){
#                         ts.dat <- subset(dat, TimeStep==t)
#
#                         points(ts.dat$MassDens,
#                                col=add_alpha(n+1, alpha=0.2),
#                                pch=16,
#                                cex=0.5)
#
#                   }
            }





            rm(dat)
            # rm(stats)

      }
}