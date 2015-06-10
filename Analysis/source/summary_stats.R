# Function to Create Summarie Stats for individual experiments
# Requires dplyr


summary_stats <- function(data){

      # Group data by Variables of interest

      tmp <- group_by(data,
                      CellCode,
                      FGroup,
                      TimeStep
                      )

      ret <- droplevels(summarize(tmp,
                                 Count=n(),
                                 Mean=mean(MassDens),
                                 meanLCI=Mean-1.96*(sd(MassDens)/sqrt(Count)),
                                 meanUCI=Mean+1.96*(sd(MassDens)/sqrt(Count)),
                                 Min=min(MassDens),
                                 Max=max(MassDens),
                                 Median=median(MassDens)
                       ))
      return(ret)


}
