
# Fitting Linear Model to decomposed time series. -------------------------



ts_trendplotID <- function(lower, upper){
      ## Maximizes the number of trophic groups in steady state by identifying
      ## ideal cutoff

res <- numeric()

      for(i in lower:upper){
            trends <- ts_test(statsFull,cutoff=i*12, expno=1:7)

            res[i-(lower-1)] <- sum(trends$Trend, na.rm=T)
            # print(i)
      }
      x11()
      plot(lower:upper,
           res,
           type="o",
           lwd=1.2,
           xlab="Time (years)",
           ylab="Non-zero slopes (n)")

      abline(v=(which(res==min(res))+(lower-1)),
             lty=2,
             col="grey70")

      ideal <- which(res==min(res))+(lower-1)
      return(ts_test(statsFull,cutoff=ideal*12, expno=1:7))

}
