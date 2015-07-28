## resampling for bootstrapping

boot_resamp <- function(dat, cutoff, nsample){

      dat.tmp <- subset(dat, TimeStep>=cutoff)

      dat.tmp <- group_by(dat.tmp,
                          ExpNo,
                          FGroup,
                          CellCode)
      res <- summarize(dat.tmp,
                              Count=n(),
                              Mean=mean(Median),
                              SD=sd(Median),
                              Min=min(Median),
                              Max=max(Median),
                              Med=median(Median),
                              medianLCI=quantile(apply(matrix(sample(Median,
                                                                     rep=TRUE,
                                                                     nsample*length(Median)),
                                                              nrow=nsample),
                                                       1,
                                                       median),
                                                 0.025),
                              medianUCI=quantile(apply(matrix(sample(Median,
                                                                     rep=TRUE,
                                                                     nsample*length(Median)),
                                                              nrow=nsample),
                                                       1,
                                                       median),
                                                 0.975, na.rm = T)

      )
      median <- tapply(res$Med, list(res$FGroup,res$ExpNo, res$CellCode), sum)

      LCI <- tapply(res$medianLCI, list(res$FGroup,res$ExpNo, res$CellCode), sum)

      UCI <- tapply(res$medianUCI, list(res$FGroup,res$ExpNo, res$CellCode), sum)

      ngroups <- length(unique(dat$FGroup))

      if(ngroups==4){
            idx <- c(1,3,4,2)
      } else if(ngroups==3){
            idx <- c(2,3,1)
      }

      out <- list(median=median[idx,,],
                  LCI=LCI[idx,,],
                  UCI=UCI[idx,,],
                  data=res
      )


}