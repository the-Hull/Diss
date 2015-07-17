# Function to Create Summarie Stats for individual experiments
# Requires dplyr


summary_statsDens <- function(data){

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
                                  Mean=mean(IndDens),
                                  SD=sd(IndDens),
                                  meanLCI=Mean-1.96*(sd(IndDens)/sqrt(Count)),
                                  meanUCI=Mean+1.96*(sd(IndDens)/sqrt(Count)),
                                  Min=min(IndDens),
                                  Max=max(IndDens),
                                  Median=median(IndDens),
                                  medianLCI=quantile(apply(matrix(sample(IndDens,
                                                                         rep=TRUE,
                                                                         nsample*length(IndDens)),
                                                                  nrow=nsample),
                                                           1,
                                                           median),
                                                     0.025),
                                  medianUCI=quantile(apply(matrix(sample(IndDens,
                                                                         rep=TRUE,
                                                                         nsample*length(IndDens)),
                                                                  nrow=nsample),
                                                           1,
                                                           median),
                                                     0.975)

      ))
      return(ret)


}
