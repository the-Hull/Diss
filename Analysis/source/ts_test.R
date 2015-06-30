
# Test For Non-zero slopes after spec. cutoff -----------------------------

## Define Cutoff.
## Fit linear model to time series (i.e. per fgroup per expno)
## Return table of ExpNo - FGroup - ZeroSlope

ts_test <- function(dat, fgroup=NULL, expno=NULL, cutoff=1){


      # Number of experiments
      if(is.null(expno)){
            exp <- as.integer(unique(dat$ExpNo))
      } else if(!is.null(expno)){
            exp <- expno}




      numexp <- length(exp)

      # Number of fgroups

      if(is.null(fgroup)){
            fgroup <- unique(dat$FGroup)
      }

      numfg <- length(fgroup)

      # Cells
      cells <- unique(dat$CellCode)
      numcell <- length(cells)

      # Results df
      numrow <- numexp*numfg*numcell

      res.tmp <- data.frame(ExpNo=integer(numrow)
                            ,FGroup=character(numrow)
                            ,CellCode=character(numrow)
                            ,pVal=numeric(numrow)
                            ,Trend=logical(numrow)
                            ,stringsAsFactors = F)


      for(ce in 1:numcell){

                  for(fg in 1:numfg){


                        # Decompose and store in object
                        tmp <- ts_decomp(dat,
                                         fgroup=fgroup[fg],
                                         cellcode=cells[ce],
                                         expno=exp,
                                         cutoff=cutoff,
                                         plot=F)[[2]]


                        for(ex in 1:numexp){

                              if(fgroup[fg]=="carnivore" & exp[ex]==8){

                                    res.tmp[(ex+((fg-1)*ex)+((ce-1)*fg*ex)), ]  <-

                                          c(exp[ex]
                                            ,fgroup[fg]
                                            ,cells[ce]
                                            ,NA
                                            ,NA
                                          )

                              } else {

                                    ts.tmp <- log10(na.omit(sapply(tmp[[exp[ex]]][3], rbind))+1)

                                    x <- 1:length(ts.tmp[,1])

                                    p.val <- summary(lm(ts.tmp[,1] ~ x))[[4]][8]

                                res.tmp[(ex+((fg-1)*numexp)+((ce-1)*numfg*numexp)), ]  <-

                                      c(exp[ex]
                                        ,fgroup[fg]
                                        ,cells[ce]
                                        ,p.val
                                        ,p.val<0.05
                                        )
                              }
                        }


                  }

            }
            res.tmp$pVal <- as.numeric(res.tmp$pVal)
            res.tmp$Trend <- as.logical(res.tmp$Trend)
            return(res.tmp)
}