# Function to Create Summarie Stats for individual experiments
# Requires dplyr


summary_stats <- function(data){

      # Group data by Variables of interest

      nsample <- 10^3

      tmp <- group_by(data,
                      ExpNo,
                      CellCode,
                      FGroup,
                      TimeStep
                      )

      ret <- droplevels(summarize(tmp,
                                 Count=n(),
                                 Mean=mean(MassDens),
                                 SD=sd(MassDens),
                                 meanLCI=Mean-1.96*(sd(MassDens)/sqrt(Count)),
                                 meanUCI=Mean+1.96*(sd(MassDens)/sqrt(Count)),
                                 Min=min(MassDens),
                                 Max=max(MassDens),
                                 Median=median(MassDens),
                                 medianLCI=quantile(apply(matrix(sample(MassDens,
                                                             rep=TRUE,
                                                             nsample*length(MassDens)),
                                                      nrow=nsample),
                                               1,
                                               median),
                                               0.025),
                                 medianUCI=quantile(apply(matrix(sample(MassDens,
                                                                      rep=TRUE,
                                                                      nsample*length(MassDens)),
                                                               nrow=nsample),
                                                        1,
                                                        median),
                                                  0.975)

                       ))
      return(ret)


}
